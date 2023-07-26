module Parser where
  data PrimaryExpr =
    Identifier String |
    FloatingConstant Double |
    IntegerConstant Integer|
    CharacterConstant Char|
    StringLiteral String |
    ParensExpr PrimaryExpr deriving Show
