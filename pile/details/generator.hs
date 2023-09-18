module Generator where
  import Data.Char
  import Data.List
  import Lexer
  import Parser
  import Syntax

  dCIdentifierToken (CDeclarator _ (CDirectDeclaratorFunctionCall (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a))) _)) = a

  edDInitDeclaratorList (CExternalDeclaration (CDeclaration _ (Just a))) = a

  edDeclarator (CFunction _ a _ _) = a

  edSCompound (CFunction _ _ _ a) = a

  generateIRType (CSpecifiers [CTypeSpecifier (CKeywordToken "void")]) Nothing = IRVoid

  generateIRType (CSpecifiers [CTypeSpecifier (CKeywordToken "int")]) Nothing = IRInteger

  generateIRType (CSpecifiers [CTypeSpecifier (CKeywordToken "char")]) Nothing = IRInteger

  generateIRType (CSpecifiers [CTypeSpecifier (CKeywordToken "float")]) Nothing = IRFloat

  generateIRType (CSpecifiers [CTypeSpecifier (CKeywordToken a)]) (Just (CPointer _)) = IRPointer (generateIRType (CSpecifiers [CTypeSpecifier (CKeywordToken a)]) Nothing)

  generateIRConstant a b
    | b == IRFloat = IRFloatingConstant (value a)
    | otherwise = IRIntegerConstant ((floor . value) a)
    where
      value (CConstant (CConstantToken (CFloatingConstant a))) = a
      value (CConstant (CConstantToken (CIntegerConstant a))) = fromIntegral a
      value (CConstant (CConstantToken (CCharacterConstant a))) = (fromIntegral . ord) a
      value _ = 0

  generateIRBasicBlock (CCompound a b) c = [IRBasicBlock "" (declarations ++ statements)]
    where
      declarationList (Just (CDeclarationList a)) = a
      declarationList Nothing = []
      declaration (CDeclaration a (Just (CInitDeclaratorList b))) = map (irAlloca a) b
      irAlloca a (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) = (Nothing, IRAlloca (generateIRType a c) Nothing Nothing)
      irAlloca a (CInitDeclarator (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) e) = (Nothing, IRAlloca (generateIRType a c) Nothing Nothing)
      declarations = map declaration (declarationList a)
      statementList (Just (CList a)) = a
      statementList Nothing = []
      statement (CReturn Nothing) = IRRet Nothing
      statement (CReturn (Just a)) = IRRet (Just (IRValue (generateIRConstant a c)))
      statements = [zip (repeat Nothing) (map statement (statementList b))]

  generateIRFunctionGlobal (CFunction (Just a) b _ c) = [IRFunctionGlobal functionType (name b) (map argument (argumentList b)) (generateIRBasicBlock c functionType)]
    where
      functionType = (generateIRType a (pointer b))
      name (CDeclarator _ (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a)))) = a
      name (CDeclarator _ (CDirectDeclaratorFunctionCall (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a))) _)) = a
      pointer (CDeclarator a _) = a
      argumentList (CDeclarator _ (CDirectDeclaratorFunctionCall _ [CParameterList a])) = a
      argument (CParameterDeclaration c d) = IRArgument (generateIRType c Nothing) (Just (name d))

  generateIRVariableGlobal (CExternalDeclaration (CDeclaration a (Just (CInitDeclaratorList b)))) = map variable b
    where
      variable (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) = IRVariableGlobal d (generateIRType a c) (generateIRConstant (CConstant (CConstantToken (CIntegerConstant 0)))(generateIRType a c))
      variable (CInitDeclarator (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) e) = IRVariableGlobal d (generateIRType a c) (generateIRConstant e (generateIRType a c))

  generateIRModule (CTranslationUnit []) a = (IRModule a)

  generateIRModule (CTranslationUnit (a:as)) b = generateIRModule (CTranslationUnit as) (function a : b)
    where
      function a = do
        case a of
          (CFunction _ _ _ _) -> generateIRFunctionGlobal a
          (CExternalDeclaration _) -> generateIRVariableGlobal a

  generateEConstant (CIntegerConstant a) = "mov r3, #" ++ show a

  generateExpression a = do
    case a of
      (CConstant (CConstantToken a)) -> do
        code <- generateEConstant a
        return code

  generateSList (Just (CList a)) = ((intercalate "\n") . (map generateStatement)) a

  generateSList Nothing = ""

  generateSCompound (CCompound a b) = intercalate "\n" [generateSList b]

  generateSReturn (Just a) = intercalate "\n" [
    generateExpression a,
    "mov r0, r3",
    "bx lr"]

  generateSReturn Nothing = "bx lr"

  generateStatement a = do
    case a of
      (CReturn a) -> do
        code <- generateSReturn a
        return code

  generateEDTranslationUnit (CTranslationUnit a) = (concat . (map function)) a
    where
      function x = do
        case x of
          (CExternalDeclaration a) -> do
            code <- generateEDDeclaration x
            return code
          (CFunction a b c d) -> do
            code <- generateEDFunction x
            return code

  generateEDDeclaration a = ((((intercalate "\n") . (map symbol) . dList . edDInitDeclaratorList) a))
    where
      symbol (CDeclarator _ (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a)))) = a ++ ":"
      symbol (CInitDeclarator a (CArithmeticOperator (COperatorToken b, c))) = intercalate "\n" [symbol a, directive c ++ b ++ value c] ++ "\n"
      symbol (CInitDeclarator a b) = intercalate "\n" [symbol a, value b] ++ "\n"
      directive (CConstant (CConstantToken (CIntegerConstant _))) = ".word "
      directive (CConstant (CConstantToken (CFloatingConstant _))) = ".word "
      directive (CConstant (CConstantToken (CCharacterConstant _))) = ".byte "
      directive (CStringLiteral (CStringLiteralToken _)) = ".ascii "
      value (CConstant (CConstantToken (CIntegerConstant a))) = show a
      value (CConstant (CConstantToken (CFloatingConstant a))) = show a
      value (CConstant (CConstantToken (CCharacterConstant a))) = (show . ord) a
      value (CStringLiteral (CStringLiteralToken a)) = "\"" ++ a ++ "\\0\""

  generateEDFunction a = intercalate "\n" [
    (dCIdentifierToken . edDeclarator) a ++ ":",
    "push {r7}",
    (generateSCompound . edSCompound) a,
    "bx lr"]

  generate a = generateEDTranslationUnit a ++ "\n"
