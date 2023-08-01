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
    StructMember Expr Expr |
    UnionMember Expr Expr|
    PostfixIncrement Expr |
    PostfixDecrement Expr deriving Show

  data UnaryExpr =
    PostfixExpr PostfixExpr |
    PrefixIncrement UnaryExpr |
    PrefixDecrement UnaryExpr |
    AddressOperator UnaryExpr |
    IndirectionOperator UnaryExpr |
    ArithmeticOperator UnaryExpr |
    SizeofOperator (Either UnaryExpr String) deriving Show

  parseToken t =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok x = if x == t then Just x else Nothing

  parseIdentifier =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok (Token pos (Lexer.Identifier x)) = Just (Primary (Parser.Identifier x))
      testTok x = Nothing

  parseFloatingConstant =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok (Token pos (Lexer.FloatingConstant x)) = Just (Primary (Parser.FloatingConstant x))
      testTok x = Nothing

  parseIntegerConstant =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok (Token pos (Lexer.IntegerConstant x)) = Just (Primary (Parser.IntegerConstant x))
      testTok x = Nothing

  parseCharacterConstant =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok (Token pos (Lexer.CharacterConstant x)) = Just (Primary (Parser.CharacterConstant x))
      testTok x = Nothing

  parseStringLiteral =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok (Token pos (Lexer.StringLiteral x)) = Just (Primary (Parser.StringLiteral x))
      testTok x = Nothing

  parsePrimaryExpr =
    parseIdentifier <|>
    parseFloatingConstant <|>
    parseIntegerConstant <|>
    parseCharacterConstant <|>
    parseStringLiteral

  parseStructMember = do
    postfixExpr <- parsePostfixExpr
    parseToken (Token Nothing (Operator ".")) <|> parseToken (Token Nothing (Operator "->"))
    identifier <- parseIdentifier
    return (Postfix (StructMember postfixExpr identifier))

  parseUnionMember = do
    postfixExpr <- parsePostfixExpr
    parseToken (Token Nothing (Operator ".")) <|> parseToken (Token Nothing (Operator "->"))
    identifier <- parseIdentifier
    return (Postfix (UnionMember postfixExpr identifier))

  parsePostfixIncrement = do
    postfixExpr <- parsePostfixExpr
    parseToken (Token Nothing (Operator "++"))
    return (Postfix (PostfixIncrement postfixExpr))

  parsePostfixDecrement = do
    postfixExpr <- parsePostfixExpr
    parseToken (Token Nothing (Operator "--"))
    return (Postfix (PostfixDecrement postfixExpr))

  parsePostfixExpr =
    parsePrimaryExpr <|>
    parseStructMember <|>
    parseUnionMember <|>
    parsePostfixIncrement <|>
    parsePostfixDecrement
