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

  generateIRType (CSpecifiers [CTypeSpecifier (CKeywordToken a)]) (Just (CPointer _)) = IRPointer (generateIRType (CSpecifiers [CTypeSpecifier (CKeywordToken a)]) Nothing)

  generateIRType (CSpecifiers [CTypeSpecifier (CKeywordToken "void")]) Nothing = IRVoid

  generateIRType (CSpecifiers [CTypeSpecifier (CKeywordToken "int")]) Nothing = IRInteger

  generateIRType (CSpecifiers [CTypeSpecifier (CKeywordToken "float")]) Nothing = IRFloat

  generateIRType (CSpecifiers [CTypeSpecifier (CKeywordToken "double")]) Nothing = IRDouble

  generateIRType (CSpecifiers [CTypeSpecifier (CKeywordToken "long")]) Nothing = IRLongDouble

  generateIRConstant (CInitDeclarator _ (CConstant (CConstantToken (CFloatingConstant a)))) = IRFloatingConstant a

  generateIRConstant (CInitDeclarator _ (CConstant (CConstantToken (CIntegerConstant a)))) = IRIntegerConstant a

  generateIRConstant (CInitDeclarator _ (CConstant (CConstantToken (CCharacterConstant a)))) = IRIntegerConstant ((fromIntegral . ord) a)

  generateIRConstant (CDeclarator _ _) = IRIntegerConstant 0

  generateIRVariableGlobal (CExternalDeclaration (CDeclaration a (Just (CInitDeclaratorList [])))) c = c

  generateIRVariableGlobal (CExternalDeclaration (CDeclaration a (Just (CInitDeclaratorList (b:bs))))) c = generateIRVariableGlobal (CExternalDeclaration (CDeclaration a (Just (CInitDeclaratorList bs)))) (IRVariableGlobal (identifier b) (generateIRType a (pointer b)) (generateIRConstant b) : c)
    where
      identifier (CInitDeclarator (CDeclarator _ (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a)))) _) = a

      identifier (CDeclarator _ (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken a)))) = a

      pointer (CInitDeclarator (CDeclarator a _) _) = a

      pointer (CDeclarator a _) = a

  generateIRModule (CTranslationUnit []) a = (IRModule a)

  generateIRModule (CTranslationUnit (a:as)) b = generateIRModule (CTranslationUnit as) (function a : b)
    where
      function a = do
        case a of
          (CExternalDeclaration _) -> generateIRVariableGlobal a []

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
