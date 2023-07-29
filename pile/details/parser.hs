module Parser where
  import Lexer

  data Expr =
    Primary PrimaryExpr |
    Postfix PostfixExpr |
    Unary UnaryExpr deriving Show

  data PrimaryExpr =
    Identifier String |
    FloatingConstant Double |
    IntegerConstant Integer|
    CharacterConstant Char|
    StringLiteral String |
    ParensExpr Expr deriving Show

  data PostfixExpr =
    PrimaryExpr PrimaryExpr |
    ArraySubscript PostfixExpr Expr |
    StructMember PostfixExpr String |
    UnionMember PostfixExpr String |
    PostfixIncrement PostfixExpr |
    PostfixDecrement PostfixExpr deriving Show

  data UnaryExpr =
    PostfixExpr PostfixExpr |
    PrefixIncrement UnaryExpr |
    PrefixDecrement UnaryExpr |
    AddressOperator UnaryExpr |
    IndirectionOperator UnaryExpr |
    ArithmeticOperator UnaryExpr |
    SizeofOperator (Either UnaryExpr String) deriving Show
