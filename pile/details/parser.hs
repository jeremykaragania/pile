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
    ArraySubscript Expr Expr |
    StructMember PostfixExpr PostfixExpr |
    UnionMember PostfixExpr PostfixExpr|
    PostfixIncrement Expr |
    PostfixDecrement Expr deriving Show

  data ArgumentExprList = ArgumentExprList [AssignmentExpr] deriving Show

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

  data AssignmentExpr =
    AssignmentValue Expr |
    AssignmentExpr Operator AssignmentExpr AssignmentExpr deriving Show

  data Declaration = Declaration DeclarationSpecifiers (Maybe InitDeclaratorList) deriving Show

  data DeclarationSpecifiers = DeclarationSpecifiers [DeclarationSpecifier] deriving Show

  data InitDeclaratorList = InitDeclaratorList [InitDeclarator] deriving Show

  data InitDeclarator = InitDeclarator Declarator deriving Show

  data DeclarationSpecifier =
    StorageClassSpecifier Keyword |
    TypeSpecifier Keyword |
    TypeQualifier Keyword deriving Show

  data Declarator = Declarator (Maybe Pointer) DirectDeclarator deriving Show

  data DirectDeclarator = DirectDeclaratorIdentifier Identifier deriving Show

  data Pointer = Pointer (Maybe TypeQualifierList) deriving Show

  data TypeQualifierList = TypeQualifierList [DeclarationSpecifier]  deriving Show

  data ParameterTypeList =
    ParamterTypeList [ParameterList] |
    ParameterTypeListElipsis [ParameterList] deriving Show

  data ParameterList = ParameterList [ParameterDeclaration] deriving Show

  data ParameterDeclaration = ParamterDeclaration deriving Show

  data IdentifierList = IdentifierList [Identifier] deriving Show

  data Statement =
    LabeledIdentifierStatement Identifier Statement |
    LabeledCaseStatement Expr Statement |
    LabeledDefaultStatement Statement |
    DeclarationList Declaration |
    StatementList Statement |
    CompoundStatement [Statement] |
    ExprStatement (Maybe Expr) |
    IfStatement Expr Statement |
    IfElseStatement Expr Statement Statement |
    SwitchStatement Expr Statement |
    WhileStatement Expr Statement |
    DoStatement Statement Expr |
    ForStatement (Maybe Expr) (Maybe Expr) (Maybe Expr) Statement |
    GotoStatement Identifier |
    ContinueStatement |
    BreakStatement |
    ReturnStatement (Maybe Expr) deriving Show

  identifierPrimaryVal (Primary (IdentifierPrimary x)) = x

  floatingConstantPrimaryVal (Primary (ConstantPrimary (FloatingConstant x))) = x

  integerConstantPrimaryVal (Primary (ConstantPrimary x)) = x

  characterConstantPrimaryVal (Primary (ConstantPrimary x)) = x

  stringLiteralPrimaryVal (Primary (StringLiteralPrimary x)) = x

  assignmentVal (Assignment x) = x

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

  parseParensExpr = do
    parseToken (Token Nothing (OperatorToken (Operator "(")))
    expr <- parseExpr
    parseToken (Token Nothing (OperatorToken (Operator ")")))
    return (Primary (ParensExprPrimary expr))

  parsePrimaryExpr =
    parseIdentifier <|>
    parseFloatingConstant <|>
    parseIntegerConstant <|>
    parseCharacterConstant <|>
    parseStringLiteral <|>
    parseParensExpr

  parseBinaryOperator :: String -> (t -> b) -> (t -> t -> t) -> (Expr -> t) -> ParsecT [Token] u Identity Expr -> ParsecT [Token] u Identity b
  parseBinaryOperator a b c d e = do
    lookAhead parseLookAhead
    expr <- chainl1 parseBinaryValue parseBinaryOperator
    return (b expr)
    where
      parseLookAhead = do
        parseBinaryValue
        parseBinaryOperator
        parseBinaryValue
      parseBinaryValue = do
        val <- e
        return (d val)
      parseBinaryOperator = do
        parseToken (Token Nothing (OperatorToken (Operator a)))
        return c

  parseArraySubscript = do
    firstExpr <- parsePrimaryExpr
    parseToken (Token Nothing (OperatorToken (Operator "[")))
    secondExpr <- parsePrimaryExpr
    parseToken (Token Nothing (OperatorToken (Operator "]")))
    return (Postfix (ArraySubscript firstExpr secondExpr))

  parseStructMember = do
    lookAhead parseLookAhead
    expr <- chainl1 parsePostfixValue parsePostfixOperator
    return (Postfix expr)
    where
      parseLookAhead = do
        parsePostfixValue
        parsePostfixOperator
        parsePostfixValue
      parsePostfixValue = do
        val <- parsePrimaryExpr
        return (PostfixValue val)
      parsePostfixOperator = do
        parseToken (Token Nothing (OperatorToken (Operator "."))) <|> parseToken (Token Nothing (OperatorToken (Operator "->")))
        return StructMember

  parseUnionMember = do
    lookAhead parseLookAhead
    expr <- chainl1 parsePostfixValue parsePostfixOperator
    return (Postfix expr)
    where
      parseLookAhead = do
        parsePostfixValue
        parsePostfixOperator
        parsePostfixValue
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
    try parseArraySubscript <|>
    try parseStructMember <|>
    try parseUnionMember <|>
    try parsePostfixIncrement <|>
    try parsePostfixDecrement <|>
    parsePrimaryExpr

  parseArgumentExprList = do
    list <- sepBy1 parseAssignmentExpr (parseToken (Token Nothing (OperatorToken (Operator ","))))
    return (ArgumentExprList (map assignmentVal list))

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
    try parsePrefixIncrement <|>
    try parsePrefixDecrement <|>
    try parseAddressOperator <|>
    try parseIndirectionOperator <|>
    try parseArithmeticOperator <|>
    parsePostfixExpr

  parseProduct = parseBinaryOperator "*" Multiplicative Product MultiplicativeValue parseUnaryExpr

  parseQuotient = parseBinaryOperator "/" Multiplicative Quotient MultiplicativeValue parseUnaryExpr

  parseRemainder = parseBinaryOperator "%" Multiplicative Remainder MultiplicativeValue parseUnaryExpr

  parseMultiplicativeExpr =
    try parseProduct <|>
    try parseQuotient <|>
    try parseRemainder <|>
    parseUnaryExpr

  parseAddition = parseBinaryOperator "+" Additive Addition AdditiveValue parseMultiplicativeExpr

  parseSubtraction = parseBinaryOperator "-" Additive Subtraction AdditiveValue parseMultiplicativeExpr

  parseAdditiveExpr =
    try parseAddition <|>
    try parseSubtraction <|>
    parseMultiplicativeExpr

  parseLeftShift = parseBinaryOperator "<<" Shift LeftShift ShiftValue parseAdditiveExpr

  parseRightShift = parseBinaryOperator ">>" Shift RightShift ShiftValue parseAdditiveExpr

  parseShiftExpr =
    try parseLeftShift <|>
    try parseRightShift <|>
    parseAdditiveExpr

  parseLesser = parseBinaryOperator "<" Relational Lesser RelationalValue parseShiftExpr

  parseGreater = parseBinaryOperator ">" Relational Greater RelationalValue parseShiftExpr

  parseLesserOrEqual = parseBinaryOperator "<=" Relational LesserOrEqual RelationalValue parseShiftExpr

  parseGreaterOrEqual = parseBinaryOperator ">=" Relational GreaterOrEqual RelationalValue parseShiftExpr

  parseRelationalExpr =
    try parseLesser <|>
    try parseGreater <|>
    try parseLesserOrEqual <|>
    try parseGreaterOrEqual <|>
    parseShiftExpr

  parseEqual = parseBinaryOperator "==" Equality Equal EqualityValue parseRelationalExpr

  parseNotEqual = parseBinaryOperator "!=" Equality NotEqual EqualityValue parseRelationalExpr

  parseEqualityExpr =
    try parseEqual <|>
    try parseNotEqual <|>
    parseRelationalExpr

  parseBitwiseAndExpr =
    try (parseBinaryOperator "&" BitwiseAnd BitwiseAndExpr BitwiseAndValue parseEqualityExpr) <|>
    parseEqualityExpr

  parseBitwiseExclusiveOrExpr =
    try (parseBinaryOperator "^" BitwiseExclusiveOr BitwiseExclusiveOrExpr BitwiseExclusiveOrValue parseEqualityExpr) <|>
    parseBitwiseAndExpr

  parseBitwiseInclusiveOrExpr =
    try (parseBinaryOperator "|" BitwiseInclusiveOr BitwiseInclusiveOrExpr BitwiseInclusiveOrValue parseEqualityExpr) <|>
    parseBitwiseExclusiveOrExpr

  parseLogicalAndExpr =
    try (parseBinaryOperator "&&" LogicalAnd LogicalAndExpr LogicalAndValue parseEqualityExpr) <|>
    parseBitwiseInclusiveOrExpr

  parseLogicalOrExpr =
    try (parseBinaryOperator "||" LogicalOr LogicalOrExpr LogicalOrValue parseEqualityExpr) <|>
    parseLogicalAndExpr

  parseConditionalExpr =
    try (
      do
        firstExpr <- parseLogicalOrExpr
        parseToken (Token Nothing (OperatorToken (Operator "?")))
        secondExpr <- parseExpr
        parseToken (Token Nothing (OperatorToken (Operator ":")))
        thirdExpr <- parseLogicalOrExpr
        return (Conditional (ConditionalExpr firstExpr secondExpr thirdExpr))) <|>
    parseLogicalOrExpr


  parseAssignmentExpr =
    try (
      do
        lookAhead parseLookAhead
        expr <- chainr1 parseAssignmentValue parseAssignmentOperator
        return (Assignment expr)) <|>
    parseConditionalExpr
    where
      parseLookAhead = do
        parseAssignmentValue
        parseAssignmentOperator
        parseAssignmentValue
      parseAssignmentValue = do
        val <- parseUnaryExpr
        return (AssignmentValue val)
      parseAssignmentOperator = do
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
        return (AssignmentExpr (operatorTokenVal (tokenVal operator)))

  parseExpr = parseAssignmentExpr

  parseDeclaration = do
    specifiers <- parseDeclarationSpecifiers
    declarator <- optionMaybe parseInitDeclaratorList
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    return (Declaration specifiers declarator)

  parseDeclarationSpecifiers = do
    specifiers <- many parseDeclarationSpecifier
    return (DeclarationSpecifiers specifiers)

  parseInitDeclaratorList = do
    list <- sepBy1 parseInitDeclarator (parseToken (Token Nothing (OperatorToken (Operator ","))))
    return (InitDeclaratorList list)

  parseInitDeclarator = do
    declarator <- parseDeclarator
    return (InitDeclarator declarator)

  parseStorageClassSpecifier = do
    specifier <-
      parseToken (Token Nothing (KeywordToken (Keyword "typedef"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "extern"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "static"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "auto"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "register")))
    return (StorageClassSpecifier (keywordTokenVal (tokenVal specifier)))

  parseTypeSpecifier = do
    specifier <-
      parseToken (Token Nothing (KeywordToken (Keyword "void"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "char"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "short"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "int"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "long"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "float"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "double"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "signed"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "unsigned")))
    return (TypeSpecifier (keywordTokenVal (tokenVal specifier)))

  parseTypeQualifier = do
    qualifier <-
      parseToken (Token Nothing (KeywordToken (Keyword "const"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "volatile")))
    return (TypeQualifier (keywordTokenVal (tokenVal qualifier)))

  parseDeclarationSpecifier =
    parseStorageClassSpecifier <|>
    parseTypeSpecifier <|>
    parseTypeQualifier

  parseDeclarator = do
    pointer <- optionMaybe parsePointer
    declarator <- parseDirectDeclarator
    return (Declarator pointer declarator)

  parseDirectDeclarator = do
    identifier <- parseIdentifier
    return (DirectDeclaratorIdentifier (identifierPrimaryVal identifier))

  parsePointer = do
    parseToken (Token Nothing (OperatorToken (Operator "*")))
    list <- parseTypeQualifierList
    case list of
      TypeQualifierList (x:xs) -> return (Pointer (Just list))
      TypeQualifierList [] -> return (Pointer Nothing)

  parseTypeQualifierList = do
    list <- many parseTypeQualifier
    return (TypeQualifierList list)

  parseIdentifierList = do
    list <- many parseIdentifier
    return (IdentifierList (map identifierPrimaryVal list))

  parseLabeledIdentifierStatement = do
    identifier <- parseIdentifier
    parseToken (Token Nothing (OperatorToken (Operator ":")))
    statement <- parseStatement
    return (LabeledIdentifierStatement (identifierPrimaryVal identifier) statement)

  parseLabeledCaseStatement = do
    parseToken (Token Nothing (KeywordToken (Keyword "case")))
    expr <- parseExpr
    parseToken (Token Nothing (OperatorToken (Operator ":")))
    statement <- parseStatement
    return (LabeledCaseStatement expr statement)

  parseLabeledDefaultStatement = do
    parseToken (Token Nothing (KeywordToken (Keyword "default")))
    parseToken (Token Nothing (OperatorToken (Operator ":")))
    statement <- parseStatement
    return (LabeledDefaultStatement statement)

  parseDeclarationList = do
    list <- parseDeclaration
    return (DeclarationList list)

  parseStatementList = do
    list <- parseStatement
    return (StatementList list)

  parseCompoundStatement = do
    parseToken (Token Nothing (PunctuatorToken (Punctuator "{")))
    list <- many (try parseDeclarationList <|> try parseStatementList)
    parseToken (Token Nothing (PunctuatorToken (Punctuator "}")))
    return (CompoundStatement list)

  parseExprStatement = do
    expr <- optionMaybe parseExpr
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    return (ExprStatement expr)

  parseIfStatement = do
    parseToken (Token Nothing (KeywordToken (Keyword "if")))
    parseToken (Token Nothing (OperatorToken (Operator "(")))
    expr <- parseExpr
    parseToken (Token Nothing (OperatorToken (Operator ")")))
    statement <- parseStatement
    return (IfStatement expr statement)

  parseIfElseStatement = do
    parseToken (Token Nothing (KeywordToken (Keyword "if")))
    parseToken (Token Nothing (OperatorToken (Operator "(")))
    expr <- parseExpr
    parseToken (Token Nothing (OperatorToken (Operator ")")))
    firstStatement <- parseStatement
    parseToken (Token Nothing (KeywordToken (Keyword "else")))
    secondStatement <- parseStatement
    return (IfElseStatement expr firstStatement secondStatement)

  parseSwitchStatement = do
    parseToken (Token Nothing (KeywordToken (Keyword "switch")))
    parseToken (Token Nothing (OperatorToken (Operator "(")))
    expr <- parseExpr
    parseToken (Token Nothing (OperatorToken (Operator ")")))
    statement <- parseStatement
    return (SwitchStatement expr statement)

  parseWhileStatement = do
    parseToken (Token Nothing (KeywordToken (Keyword "while")))
    parseToken (Token Nothing (OperatorToken (Operator "(")))
    expr <- parseExpr
    parseToken (Token Nothing (OperatorToken (Operator ")")))
    statement <- parseStatement
    return (WhileStatement expr statement)

  parseDoStatement = do
    parseToken (Token Nothing (KeywordToken (Keyword "do")))
    statement <- parseStatement
    parseToken (Token Nothing (KeywordToken (Keyword "while")))
    parseToken (Token Nothing (OperatorToken (Operator "(")))
    expr <- parseExpr
    parseToken (Token Nothing (OperatorToken (Operator ")")))
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    return (DoStatement statement expr)

  parseForStatement = do
    parseToken (Token Nothing (KeywordToken (Keyword "for")))
    parseToken (Token Nothing (OperatorToken (Operator "(")))
    firstExpr <- optionMaybe parseExpr
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    secondExpr <- optionMaybe parseExpr
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    thirdExpr <- optionMaybe parseExpr
    parseToken (Token Nothing (OperatorToken (Operator ")")))
    statement <- parseStatement
    return (ForStatement firstExpr secondExpr thirdExpr statement)

  parseGotoStatement = do
    parseToken (Token Nothing (KeywordToken (Keyword "goto")))
    identifier <- parseIdentifier
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    return (GotoStatement (identifierPrimaryVal identifier))

  parseContinueStatement = do
    parseToken (Token Nothing (KeywordToken (Keyword "continue")))
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    return (ContinueStatement)

  parseBreakStatement = do
    parseToken (Token Nothing (KeywordToken (Keyword "break")))
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    return (BreakStatement)

  parseReturnStatement = do
    parseToken (Token Nothing (KeywordToken (Keyword "return")))
    expr <- optionMaybe parseExpr
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    return (ReturnStatement expr)

  parseStatement =
    try parseLabeledIdentifierStatement <|>
    parseLabeledCaseStatement <|>
    parseLabeledDefaultStatement <|>
    parseCompoundStatement <|>
    parseExprStatement <|>
    try parseIfElseStatement <|>
    parseIfStatement <|>
    parseSwitchStatement <|>
    parseWhileStatement <|>
    parseDoStatement <|>
    parseForStatement <|>
    parseGotoStatement <|>
    parseContinueStatement <|>
    parseBreakStatement <|>
    parseReturnStatement

  parse = Text.Parsec.parse (many parseStatement) ""
