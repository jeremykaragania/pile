module Parser where
  import Lexer
  import Data.Functor.Identity
  import Text.Parsec

  data Expr =
    Primary PrimaryExpr |
    Postfix PostfixExpr |
    Unary UnaryExpr |
    Multiplicative MultiplicativeExpr |
    Additive AdditiveExpr |
    Shift ShiftExpr |
    Relational RelationalExpr |
    Equality EqualityExpr |
    BitwiseAnd BitwiseAndExpr |
    BitwiseExclusiveOr BitwiseExclusiveOrExpr |
    BitwiseInclusiveOr BitwiseInclusiveOrExpr |
    LogicalAnd LogicalAndExpr |
    LogicalOr LogicalOrExpr |
    Conditional ConditionalExpr |
    Assignment AssignmentExpr deriving Show

  data PrimaryExpr =
    IdentifierPrimary Identifier |
    ConstantPrimary Constant |
    StringLiteralPrimary StringLiteral |
    ParensExprPrimary Expr deriving Show

  data PostfixExpr =
    PostfixValue Expr |
    ArraySubscript PostfixExpr Expr |
    StructMember PostfixExpr PostfixExpr |
    UnionMember PostfixExpr PostfixExpr|
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
    MultiplicativeValue Expr |
    Product MultiplicativeExpr MultiplicativeExpr |
    Quotient MultiplicativeExpr MultiplicativeExpr |
    Remainder MultiplicativeExpr MultiplicativeExpr deriving Show

  data AdditiveExpr =
    AdditiveValue Expr |
    Addition AdditiveExpr AdditiveExpr |
    Subtraction AdditiveExpr AdditiveExpr deriving Show

  data ShiftExpr =
    ShiftValue Expr |
    LeftShift ShiftExpr ShiftExpr |
    RightShift ShiftExpr ShiftExpr deriving Show

  data RelationalExpr =
    RelationalValue Expr |
    Lesser RelationalExpr RelationalExpr |
    Greater RelationalExpr RelationalExpr |
    LesserOrEqual RelationalExpr RelationalExpr |
    GreaterOrEqual RelationalExpr RelationalExpr deriving Show

  data EqualityExpr =
    EqualityValue Expr |
    Equal EqualityExpr EqualityExpr |
    NotEqual EqualityExpr EqualityExpr deriving Show

  data BitwiseAndExpr =
    BitwiseAndValue Expr |
    BitwiseAndExpr BitwiseAndExpr BitwiseAndExpr deriving Show

  data BitwiseExclusiveOrExpr =
    BitwiseExclusiveOrValue Expr |
    BitwiseExclusiveOrExpr BitwiseExclusiveOrExpr BitwiseExclusiveOrExpr deriving Show

  data BitwiseInclusiveOrExpr =
    BitwiseInclusiveOrValue Expr |
    BitwiseInclusiveOrExpr BitwiseInclusiveOrExpr BitwiseInclusiveOrExpr deriving Show

  data LogicalAndExpr =
    LogicalAndValue Expr |
    LogicalAndExpr LogicalAndExpr LogicalAndExpr deriving Show

  data LogicalOrExpr =
    LogicalOrValue Expr |
    LogicalOrExpr LogicalOrExpr LogicalOrExpr deriving Show

  data ConditionalExpr = ConditionalExpr Expr Expr Expr deriving Show

  data AssignmentExpr = AssignmentExpr Expr TokenValue Expr deriving Show

  data Declaration = Declaration DeclarationSpecifiers (Maybe InitDeclaratorList) deriving Show

  data DeclarationSpecifiers =
    StorageClassSpecifiers StorageClassSpecifier (Maybe DeclarationSpecifiers) |
    TypeSpecifiers TypeSpecifier (Maybe DeclarationSpecifiers) |
    TypeQualifiers TypeQualifier (Maybe DeclarationSpecifiers) deriving Show

  data InitDeclaratorList = InitDeclaratorList [InitDeclarator] deriving Show

  data InitDeclarator = InitDeclarator Declarator deriving Show

  data StorageClassSpecifier = StorageClassSpecifier TokenValue deriving Show

  data TypeSpecifier = TypeSpecifier TokenValue deriving Show

  data TypeQualifier = TypeQualifier TokenValue deriving Show

  data Declarator = Declarator (Maybe Pointer) DirectDeclarator deriving Show

  data DirectDeclarator = DirectDeclaratorIdentifier TokenValue deriving Show

  data Pointer = Pointer [(Maybe TypeQualifierList)] deriving Show

  data TypeQualifierList = TypeQualifierList [TypeQualifier]  deriving Show

  data ParameterTypeList =
    ParamterTypeList [ParameterList] |
    ParameterTypeListElipsis [ParameterList] deriving Show

  data ParameterList = ParameterList [ParameterDeclaration] deriving Show

  data ParameterDeclaration = ParamterDeclaration deriving Show

  data IdentifierList = IdentifierList [TokenValue]

  identifierVal (Primary (IdentifierPrimary (Identifier x))) = x

  floatingConstantVal (Primary (ConstantPrimary (FloatingConstant x))) = x

  integerConstantVal (Primary (ConstantPrimary (IntegerConstant x))) = x

  characterConstantVal (Primary (ConstantPrimary (CharacterConstant x))) = x

  stringLiteralVal (Primary (StringLiteralPrimary (StringLiteral x))) = x

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
      testTok (Token pos (IdentifierToken (Identifier x))) = Just (Primary (IdentifierPrimary (Identifier x)))
      testTok x = Nothing

  parseFloatingConstant =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok (Token pos (ConstantToken (FloatingConstant x))) = Just (Primary (ConstantPrimary (FloatingConstant x)))
      testTok x = Nothing

  parseIntegerConstant =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok (Token pos (ConstantToken (IntegerConstant x))) = Just (Primary (ConstantPrimary (IntegerConstant x)))
      testTok x = Nothing

  parseCharacterConstant =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok (Token pos (ConstantToken (CharacterConstant x))) = Just (Primary (ConstantPrimary (CharacterConstant x)))
      testTok x = Nothing

  parseStringLiteral =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok (Token pos (StringLiteralToken (StringLiteral x))) = Just (Primary (StringLiteralPrimary (StringLiteral x)))
      testTok x = Nothing

  parsePrimaryExpr =
    parseIdentifier <|>
    parseFloatingConstant <|>
    parseIntegerConstant <|>
    parseCharacterConstant <|>
    parseStringLiteral

  parseBinaryOperator :: String -> (t -> b) -> (t -> t -> t) -> (Expr -> t) -> ParsecT [Token] u Identity Expr -> ParsecT [Token] u Identity b
  parseBinaryOperator a b c d e = do
    expr <- chainl1 parseBinaryValue parseBinaryOperator
    return (b expr)
    where
      parseBinaryValue = do
        val <- e
        return (d val)
      parseBinaryOperator = do
        parseToken (Token Nothing (OperatorToken (Operator a)))
        return c

  parseStructMember = do
    expr <- chainl1 parsePostfixValue parsePostfixOperator
    return (Postfix expr)
    where
      parsePostfixValue = do
        val <- parsePrimaryExpr
        return (PostfixValue val)
      parsePostfixOperator = do
        parseToken (Token Nothing (OperatorToken (Operator "."))) <|> parseToken (Token Nothing (OperatorToken (Operator "->")))
        return StructMember

  parseUnionMember = do
    expr <- chainl1 parsePostfixValue parsePostfixOperator
    return (Postfix expr)
    where
      parsePostfixValue = do
        val <- parsePrimaryExpr
        return (PostfixValue val)
      parsePostfixOperator = do
        parseToken (Token Nothing (OperatorToken (Operator "."))) <|> parseToken (Token Nothing (OperatorToken (Operator "->")))
        return UnionMember

  parsePostfixIncrement = do
    expr <- parsePrimaryExpr
    parseToken (Token Nothing (OperatorToken (Operator "++")))
    return (Postfix (PostfixIncrement expr))

  parsePostfixDecrement = do
    expr <- parsePrimaryExpr
    parseToken (Token Nothing (OperatorToken (Operator "--")))
    return (Postfix (PostfixDecrement expr))

  parsePostfixExpr =
    parseStructMember <|>
    parseUnionMember <|>
    parsePostfixIncrement <|>
    parsePostfixDecrement

  parsePrefixIncrement = do
    parseToken (Token Nothing (OperatorToken (Operator "++")))
    expr <- parsePostfixExpr
    return (Unary (PrefixIncrement expr))

  parsePrefixDecrement = do
    parseToken (Token Nothing (OperatorToken (Operator "--")))
    expr <- parsePostfixExpr
    return (Unary (PrefixDecrement expr))

  parseAddressOperator = do
    parseToken (Token Nothing (OperatorToken (Operator "&")))
    expr <- parsePostfixExpr
    return (Unary (AddressOperator expr))

  parseIndirectionOperator = do
    parseToken (Token Nothing (OperatorToken (Operator "*")))
    expr <- parsePostfixExpr
    return (Unary (IndirectionOperator expr))

  parseArithmeticOperator = do
    parseToken (Token Nothing (OperatorToken (Operator "+"))) <|>
      parseToken (Token Nothing (OperatorToken (Operator "-"))) <|>
      parseToken (Token Nothing (OperatorToken (Operator "~"))) <|>
      parseToken (Token Nothing (OperatorToken (Operator "!")))
    expr <- parsePostfixExpr
    return (Unary (ArithmeticOperator expr))

  parseUnaryExpr =
    parsePrefixIncrement <|>
    parsePrefixDecrement <|>
    parseAddressOperator <|>
    parseIndirectionOperator <|>
    parseArithmeticOperator <|>
    parsePostfixExpr

  parseProduct = parseBinaryOperator "*" Multiplicative Product MultiplicativeValue parseUnaryExpr

  parseQuotient = parseBinaryOperator "/" Multiplicative Quotient MultiplicativeValue parseUnaryExpr

  parseRemainder = parseBinaryOperator "%" Multiplicative Remainder MultiplicativeValue parseUnaryExpr

  parseMultiplicativeExpr =
    parseProduct <|>
    parseQuotient <|>
    parseRemainder

  parseAddition = parseBinaryOperator "+" Additive Addition AdditiveValue parseMultiplicativeExpr

  parseSubtraction = parseBinaryOperator "-" Additive Subtraction AdditiveValue parseMultiplicativeExpr

  parseAdditiveExpr =
    parseAddition <|>
    parseSubtraction

  parseLeftShift = parseBinaryOperator "<<" Shift LeftShift ShiftValue parseAdditiveExpr

  parseRightShift = parseBinaryOperator ">>" Shift RightShift ShiftValue parseAdditiveExpr

  parseShiftExpr =
    parseLeftShift <|>
    parseRightShift

  parseLesser = parseBinaryOperator "<" Relational Lesser RelationalValue parseShiftExpr

  parseGreater = parseBinaryOperator ">" Relational Greater RelationalValue parseShiftExpr

  parseLesserOrEqual = parseBinaryOperator "<=" Relational LesserOrEqual RelationalValue parseShiftExpr

  parseGreaterOrEqual = parseBinaryOperator ">=" Relational GreaterOrEqual RelationalValue parseShiftExpr

  parseRelationalExpr =
    parseLesser <|>
    parseGreater <|>
    parseLesserOrEqual <|>
    parseGreaterOrEqual

  parseEqual = parseBinaryOperator "==" Equality Equal EqualityValue parseRelationalExpr

  parseNotEqual = parseBinaryOperator "!=" Equality NotEqual EqualityValue parseRelationalExpr

  parseEqualityExpr =
    parseEqual <|>
    parseNotEqual

  parseBitwiseAndExpr = parseBinaryOperator "&" BitwiseAnd BitwiseAndExpr BitwiseAndValue parseEqualityExpr

  parseBitwiseExclusiveOrExpr = parseBinaryOperator "^" BitwiseExclusiveOr BitwiseExclusiveOrExpr BitwiseExclusiveOrValue parseEqualityExpr

  parseBitwiseInclusiveOrExpr = parseBinaryOperator "|" BitwiseInclusiveOr BitwiseInclusiveOrExpr BitwiseInclusiveOrValue parseEqualityExpr

  parseLogicalAndExpr = parseBinaryOperator "&&" LogicalAnd LogicalAndExpr LogicalAndValue parseEqualityExpr

  parseLogicalOrExpr = parseBinaryOperator "||" LogicalOr LogicalOrExpr LogicalOrValue parseEqualityExpr

  parseConditionalExpr = do
    firstExpr <- parseLogicalOrExpr
    parseToken (Token Nothing (OperatorToken (Operator "?")))
    secondExpr <- parseExpr
    parseToken (Token Nothing (OperatorToken (Operator ":")))
    thirdExpr <- parseLogicalOrExpr
    return (Conditional (ConditionalExpr firstExpr secondExpr thirdExpr))

  parseAssignmentExpr = do
    lhs <- parseUnaryExpr
    operator <-
      parseToken (Token Nothing (OperatorToken (Operator "="))) <|>
      parseToken (Token Nothing (OperatorToken (Operator "*="))) <|>
      parseToken (Token Nothing (OperatorToken (Operator "/="))) <|>
      parseToken (Token Nothing (OperatorToken (Operator "%="))) <|>
      parseToken (Token Nothing (OperatorToken (Operator "+="))) <|>
      parseToken (Token Nothing (OperatorToken (Operator "-="))) <|>
      parseToken (Token Nothing (OperatorToken (Operator "<<="))) <|>
      parseToken (Token Nothing (OperatorToken (Operator ">>="))) <|>
      parseToken (Token Nothing (OperatorToken (Operator "&="))) <|>
      parseToken (Token Nothing (OperatorToken (Operator "^="))) <|>
      parseToken (Token Nothing (OperatorToken (Operator "|=")))
    rhs <- parseUnaryExpr
    return (Assignment (AssignmentExpr lhs (tokenVal operator) rhs))

  parseExpr =
    try parseAssignmentExpr <|>
    try parseConditionalExpr <|>
    try parseLogicalOrExpr <|>
    try parseLogicalAndExpr <|>
    try parseBitwiseInclusiveOrExpr <|>
    try parseBitwiseExclusiveOrExpr <|>
    try parseBitwiseAndExpr <|>
    try parseEqualityExpr <|>
    try parseRelationalExpr <|>
    try parseShiftExpr <|>
    try parseAdditiveExpr <|>
    try parseMultiplicativeExpr <|>
    try parseUnaryExpr <|>
    try parsePostfixExpr <|>
    try parsePrimaryExpr

  parse = Text.Parsec.parse (many parseExpr) ""
