module Parser where
  import Lexer
  import Text.Parsec

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

  parseIdentifier =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok (Token pos (Lexer.Identifier x)) = Just (Parser.Identifier x)
      testTok x = Nothing

  parseFloatingConstant =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok (Token pos (Lexer.FloatingConstant x)) = Just (Parser.FloatingConstant x)
      testTok x = Nothing

  parseIntegerConstant =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok (Token pos (Lexer.IntegerConstant x)) = Just (Parser.IntegerConstant x)
      testTok x = Nothing

  parseCharacterConstant =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok (Token pos (Lexer.CharacterConstant x)) = Just (Parser.CharacterConstant x)
      testTok x = Nothing

  parseStringLiteral =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok (Token pos (Lexer.StringLiteral x)) = Just (Parser.StringLiteral x)
      testTok x = Nothing

  parsePrimaryExpr =
    parseIdentifier <|>
    parseFloatingConstant <|>
    parseIntegerConstant <|>
    parseCharacterConstant <|>
    parseStringLiteral
