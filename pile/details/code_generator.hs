module CodeGenerator where
  import Data.List
  import Lexer
  import Parser

  edIdentifier (EDDeclaration a) = ""

  edIdentifier (EDFunction _ (DDeclarator _ (DDirectDeclaratorFunctionCall (DDirectDeclaratorIdentifier (EIdentifier a)) _)) _ _) = identifier a

  edSCompound (EDFunction _ _ _ a) = a

  generateEConstant (IntegerConstant a) = "mov r3, #" ++ show a

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
          (EDFunction a b c d) -> do
            code <- generateEDFunction x
            return code

  generateEDFunction a = intercalate "\n" [
    edIdentifier a ++ ":",
    "push {r7}",
    (generateSCompound . edSCompound) a,
    "bx lr"]

  generate a = generateEDTranslationUnit a
