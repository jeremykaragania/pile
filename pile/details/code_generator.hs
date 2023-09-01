module CodeGenerator where
  import Lexer
  import Parser

  edIdentifier (EDDeclaration a) = ""

  edIdentifier (EDFunction _ (DDeclarator _ (DDirectDeclaratorFunctionCall (DDirectDeclaratorIdentifier (EIdentifier a)) _)) _ _) = identifier a

  edSCompound (EDFunction _ _ _ a) = a
