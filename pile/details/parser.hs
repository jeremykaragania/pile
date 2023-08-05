module Parser where
  import Lexer
  import Text.Parsec

  data Expr =
    Primary PrimaryExpr |
    Postfix PostfixExpr |
    Unary UnaryExpr |
    Multiplicative MultiplicativeExpr |
    Additive AdditiveExpr |
    Shift ShiftExpr |
    Relational RelationalExpr |
    Equality EqualityExpr deriving Show

  data PrimaryExpr =
    Identifier String |
    FloatingConstant Double |
    IntegerConstant Integer|
    CharacterConstant Char|
    StringLiteral String |
    ParensExpr Expr deriving Show

  data PostfixExpr =
    ArraySubscript PostfixExpr Expr |
    StructMember Expr Expr |
    UnionMember Expr Expr|
    PostfixIncrement Expr |
    PostfixDecrement Expr deriving Show

  data UnaryExpr =
    PrefixIncrement Expr |
    PrefixDecrement Expr |
    AddressOperator Expr |
    IndirectionOperator Expr |
    ArithmeticOperator Expr |
    SizeofOperator (Either UnaryExpr String) deriving Show

  data MultiplicativeExpr =
    Product Expr Expr |
    Quotient Expr Expr |
    Remainder Expr Expr deriving Show

  data AdditiveExpr =
    Addition Expr Expr |
    Subtraction Expr Expr deriving Show

  data ShiftExpr =
    LeftShift Expr Expr |
    RightShift Expr Expr deriving Show

  data RelationalExpr =
    Lesser Expr Expr |
    Greater Expr Expr |
    LesserOrEqual Expr Expr |
    GreaterOrEqual Expr Expr deriving Show

  data EqualityExpr =
    Equal Expr Expr |
    NotEqual Expr Expr deriving Show

  identifierVal (Primary (Parser.Identifier x)) = x

  floatingConstantVal (Primary (Parser.FloatingConstant x)) = x

  integerConstantVal (Primary (Parser.IntegerConstant x)) = x

  characterConstantVal (Primary (Parser.CharacterConstant x)) = x

  stringLiteralVal (Primary (Parser.StringLiteral x)) = x

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
    try parseIdentifier <|>
    try parseFloatingConstant <|>
    try parseIntegerConstant <|>
    try parseCharacterConstant <|>
    try parseStringLiteral

  parseBinaryOperator a b c = do
    lhs <- parsePrimaryExpr
    parseToken (Token Nothing (Operator a))
    rhs <- parsePrimaryExpr
    return (b (c lhs rhs))

  parseStructMember = do
    expr <- parsePrimaryExpr
    parseToken (Token Nothing (Operator ".")) <|> parseToken (Token Nothing (Operator "->"))
    identifier <- parseIdentifier
    return (Postfix (StructMember expr identifier))

  parseUnionMember = do
    expr <- parsePrimaryExpr
    parseToken (Token Nothing (Operator ".")) <|> parseToken (Token Nothing (Operator "->"))
    identifier <- parseIdentifier
    return (Postfix (UnionMember expr identifier))

  parsePostfixIncrement = do
    expr <- parsePrimaryExpr
    parseToken (Token Nothing (Operator "++"))
    return (Postfix (PostfixIncrement expr))

  parsePostfixDecrement = do
    expr <- parsePrimaryExpr
    parseToken (Token Nothing (Operator "--"))
    return (Postfix (PostfixDecrement expr))

  parsePostfixExpr =
    try parseStructMember <|>
    try parseUnionMember <|>
    try parsePostfixIncrement <|>
    try parsePostfixDecrement

  parsePrefixIncrement = do
    parseToken (Token Nothing (Operator "++"))
    expr <- parsePrimaryExpr
    return (Unary (PrefixIncrement expr))

  parsePrefixDecrement = do
    parseToken (Token Nothing (Operator "--"))
    expr <- parsePrimaryExpr
    return (Unary (PrefixDecrement expr))

  parseAddressOperator = do
    parseToken (Token Nothing (Operator "&"))
    expr <- parsePrimaryExpr
    return (Unary (AddressOperator expr))

  parseIndirectionOperator = do
    parseToken (Token Nothing (Operator "*"))
    expr <- parsePrimaryExpr
    return (Unary (IndirectionOperator expr))

  parseArithmeticOperator = do
    parseToken (Token Nothing (Operator "+")) <|> parseToken (Token Nothing (Operator "-")) <|> parseToken (Token Nothing (Operator "~")) <|> parseToken (Token Nothing (Operator "!"))
    expr <- parsePrimaryExpr
    return (Unary (ArithmeticOperator expr))

  parseUnaryExpr =
    try parsePrefixIncrement <|>
    try parsePrefixDecrement <|>
    try parseAddressOperator <|>
    try parseIndirectionOperator <|>
    try parseArithmeticOperator

  parseProduct = parseBinaryOperator "*" Multiplicative Product

  parseQuotient = parseBinaryOperator "/" Multiplicative Quotient

  parseRemainder = parseBinaryOperator "%" Multiplicative Remainder

  parseMultiplicativeExpr =
    try parseProduct <|>
    try parseQuotient <|>
    try parseRemainder

  parseAddition = parseBinaryOperator "+" Additive Addition

  parseSubtraction = parseBinaryOperator "-" Additive Subtraction

  parseAdditiveExpr =
    try parseAddition <|>
    try parseSubtraction

  parseLeftShift = parseBinaryOperator "<<" Shift LeftShift

  parseRightShift = parseBinaryOperator ">>" Shift RightShift

  parseShiftExpr =
    try parseLeftShift <|>
    try parseRightShift

  parseLesser = parseBinaryOperator "<" Relational Lesser

  parseGreater = parseBinaryOperator ">" Relational Greater

  parseLesserOrEqual = parseBinaryOperator "<=" Relational LesserOrEqual

  parseGreaterOrEqual = parseBinaryOperator ">=" Relational GreaterOrEqual

  parseRelationalExpr =
    try parseLesser <|>
    try parseGreater <|>
    try parseLesserOrEqual <|>
    try parseGreaterOrEqual

  parseEqual = parseBinaryOperator "==" Equality Equal

  parseNotEqual = parseBinaryOperator "!=" Equality NotEqual

  parseEqualityExpr =
    try parseEqual <|>
    try parseNotEqual

  parseExpr =
    try parseEqualityExpr <|>
    try parseRelationalExpr <|>
    try parseShiftExpr <|>
    try parseAdditiveExpr <|>
    try parseMultiplicativeExpr <|>
    try parseUnaryExpr <|>
    try parsePostfixExpr <|>
    try parsePrimaryExpr
