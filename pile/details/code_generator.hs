module CodeGenerator where
  import Data.Char
  import Data.List
  import Lexer
  import Parser
  import Syntax

  dCIdentifier (DDeclarator _ (DDirectDeclaratorFunctionCall (DDirectDeclaratorIdentifier (EIdentifier (CIdentifier a))) _)) = a

  edDInitDeclaratorList (EDDeclaration (DDeclaration _ (Just a))) = a

  edDeclarator (EDFunction _ a _ _) = a

  edSCompound (EDFunction _ _ _ a) = a

  generateEConstant (CIntegerConstant a) = "mov r3, #" ++ show a

  generateExpression a = do
    case a of
      (EConstant a) -> do
        code <- generateEConstant a
        return code

  generateSList (Just (SList a)) = ((intercalate "\n") . (map generateStatement)) a

  generateSList Nothing = ""

  generateSCompound (SCompound a b) = intercalate "\n" [generateSList b]

  generateSReturn (Just a) = intercalate "\n" [
    generateExpression a,
    "mov r0, r3",
    "bx lr"]

  generateSReturn Nothing = "bx lr"

  generateStatement a = do
    case a of
      (SReturn a) -> do
        code <- generateSReturn a
        return code

  generateEDTranslationUnit (EDTranslationUnit a) = (concat . (map function)) a
    where 
      function x = do
        case x of
          (EDDeclaration a) -> do
            code <- generateEDDeclaration x
            return code
          (EDFunction a b c d) -> do
            code <- generateEDFunction x
            return code

  generateEDDeclaration a = ((((intercalate "\n") . (map symbol) . dList . edDInitDeclaratorList) a))
    where
      symbol (DDeclarator _ (DDirectDeclaratorIdentifier (EIdentifier (CIdentifier a)))) = a ++ ":"

      symbol (DInitDeclarator a (EArithmeticOperator (COperator b, c))) = intercalate "\n" [symbol a, directive c ++ b ++ value c] ++ "\n"

      symbol (DInitDeclarator a b) = intercalate "\n" [symbol a, value b] ++ "\n"

      directive (EConstant (CIntegerConstant _)) = ".word "

      directive (EConstant (CFloatingConstant _)) = ".word "

      directive (EConstant (CCharacterConstant _)) = ".byte "

      directive (EStringLiteral (CStringLiteral _)) = ".ascii "

      value (EConstant (CFloatingConstant a)) = show a

      value (EConstant (CIntegerConstant a)) = show a

      value (EConstant (CCharacterConstant a)) = (show . ord) a

      value (EStringLiteral (CStringLiteral a)) = "\"" ++ a ++ "\\0\""

  generateEDFunction a = intercalate "\n" [
    (dCIdentifier . edDeclarator) a ++ ":",
    "push {r7}",
    (generateSCompound . edSCompound) a,
    "bx lr"]

  generate a = generateEDTranslationUnit a ++ "\n"
