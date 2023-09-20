module Generator where
  import Data.Char
  import Data.List
  import Lexer
  import Parser
  import Syntax

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
      declaration (CDeclaration a (Just (CInitDeclaratorList b))) = map (irAlloca a) b ++ map (irStore a) b
      irAlloca a (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) = (Nothing, IRAlloca (generateIRType a c) Nothing Nothing)
      irAlloca a (CInitDeclarator (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) e) = (Nothing, IRAlloca (generateIRType a c) Nothing Nothing)
      irStore a (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) = (Nothing, IRStore (IRValue (generateIRConstant (CConstant (CConstantToken (CIntegerConstant 0))) (generateIRType a c))) (IRValue IRNullPointerConstant) Nothing)
      irStore a (CInitDeclarator (CDeclarator c (CDirectDeclaratorIdentifier (CIdentifier (CIdentifierToken d)))) e) = (Nothing, IRStore (IRValue (generateIRConstant e (generateIRType a c))) (IRValue IRNullPointerConstant) Nothing)
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

  generateIRModule (CTranslationUnit a) = IRModule (concat (map cExternalDefinition a))
    where
      cExternalDefinition (CFunction c d e f) = generateIRFunctionGlobal (CFunction c d e f)
      cExternalDefinition (CExternalDeclaration c) = generateIRVariableGlobal (CExternalDeclaration c)

  generateIRModuleCode (IRModule a) = intercalate "\n" (map (intercalate "\n" . irGlobalValueCode) a)
    where
      irGlobalValueCode (IRFunctionGlobal c d e f) = [d ++ ":", "\tpush {r7}", "\tbx lr"]
      irGlobalValueCode (IRVariableGlobal c d e) = [c ++ ":", "\t" ++ directive d ++ value e]
      directive IRFloat = ".float "
      directive IRInteger = ".word "
      value (IRFloatingConstant a) = show a
      value (IRIntegerConstant a) = show a

  generate a = (generateIRModuleCode . generateIRModule) a
