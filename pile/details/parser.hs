module Parser where
  import Lexer
  import Data.Functor.Identity
  import Text.Parsec

  data Expression =
    EIdentifier Identifier |
    EConstant Constant |
    EStringLiteral StringLiteral |
    EParens Expression |
    EArraySubscript Expression [Expression] |
    EFunctionCall Expression [(Maybe Expression)] |
    EStructOrUnionMember Expression [Expression] |
    EPostfixIncrement Expression |
    EPostfixDecrement Expression |
    EArgumentList [Expression] |
    EPrefixIncrement Expression |
    EPrefixDecrement Expression |
    EAddressOperator Expression |
    EIndirectionOperator Expression |
    EArithmeticOperator (Operator, Expression) |
    ESizeofEUnary Expression |
    ESizeofTypeName Declaration |
    ECast Declaration Expression |
    EProduct Expression [Expression] |
    EQuotient Expression [Expression] |
    ERemainder Expression [Expression] |
    EAddition Expression [Expression] |
    ESubtraction Expression [Expression] |
    ELeftShift Expression [Expression] |
    ERightShift Expression [Expression] |
    ELesser Expression [Expression] |
    EGreater Expression [Expression] |
    ELesserOrEqual Expression [Expression] |
    EGreaterOrEqual Expression [Expression] |
    EEqual Expression [Expression] |
    ENotEqual Expression [Expression] |
    EBitwiseAnd Expression [Expression] |
    EBitwiseExclusiveOr Expression [Expression] |
    EBitwiseInclusiveOr Expression [Expression] |
    ELogicalAnd Expression [Expression] |
    ELogicalOr Expression [Expression] |
    EConditional Expression Expression Expression |
    EAssignment Expression [(Operator, Expression)] deriving Show

  data Declaration =
    DDeclaration Declaration (Maybe Declaration) |
    DSpecifiers [Declaration] |
    DInitDeclaratorList [Declaration] |
    DInitDeclarator Declaration Expression |
    DStorageClassSpecifier Keyword |
    DTypeSpecifier Keyword |
    DStructOrUnionSpecifierComplete (Maybe Identifier) Declaration |
    DStructOrUnionSpecifierIncomplete Identifier |
    DStructOrUnion Keyword |
    DStructDeclarationList [Declaration] |
    DStructDeclaration Declaration Declaration |
    DSpecifierQualifierList [Declaration] |
    DStructDeclaratorList [Declaration] |
    DStructDeclarator (Maybe Declaration) Expression |
    DEnumSpecifierComplete (Maybe Identifier) Declaration |
    DEnumSpecifierIncomplete Identifier |
    DEnumeratorList [Declaration] |
    DEnumerator Expression Expression |
    DTypeQualifier Keyword |
    DDeclarator (Maybe Declaration) Declaration |
    DDirectDeclaratorIdentifier Expression |
    DDirectDeclaratorParens Declaration |
    DDirectDeclaratorArraySubscript Declaration |
    DDirectDeclaratorFunctionCall Declaration [Declaration] |
    DPointer (Maybe Declaration) |
    DTypeQualifierList [Declaration] |
    DParameterTypeList [Declaration] |
    DParameterList [Declaration] |
    DParameterDeclaration Declaration Declaration |
    DParameterDeclarationAbstract Declaration (Maybe Declaration) |
    DIdentifierList [Expression] |
    DTypeName Declaration (Maybe Declaration) |
    DAbstractDeclarator (Maybe Declaration) Declaration |
    DDirectAbstractDeclaratorParens Declaration |
    DDirectAbstractDeclaratorArraySubscript Declaration |
    DDirectAbstractDeclaratorFunctionCall Declaration |
    DTypedefName Expression |
    DInitializer (Either Expression Declaration) |
    DInitializerList [Declaration] deriving Show

  data Statement =
    SLabeledIdentifier Expression Statement |
    SLabeledCase Expression Statement |
    SLabeledDefault Statement |
    SDeclarationList [Declaration] |
    SList [Statement] |
    SCompound (Maybe Statement) (Maybe Statement) |
    SExpression (Maybe Expression) |
    SIf Expression Statement |
    SIfElse Expression Statement Statement |
    SSwitch Expression Statement |
    SWhile Expression Statement |
    SDo Statement Expression |
    SFor (Maybe Expression) (Maybe Expression) (Maybe Expression) Statement |
    SGoto Expression |
    SContinue |
    SBreak |
    SReturn (Maybe Expression) deriving Show

  data ExternalDefinition =
    EDTranslationUnit [ExternalDefinition] |
    EDDeclaration Declaration |
    EDFunction (Maybe Declaration) Declaration (Maybe Statement) Statement deriving Show

  parseToken t =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos
      testTok x = if x == t then Just x else Nothing

  parseTokenValue testTok =
    tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos x xs = pos

  parseIdentifierToken =
    parseTokenValue testTok
    where
      testTok (Token pos (IdentifierToken x)) = Just x
      testTok x = Nothing

  parseConstantToken =
    parseTokenValue testTok
    where
      testTok (Token pos (ConstantToken x)) = Just x
      testTok x = Nothing

  parseStringLiteralToken =
    parseTokenValue testTok
    where
      testTok (Token pos (StringLiteralToken x)) = Just x
      testTok x = Nothing

  parseEIdentifier = do
    expr <- parseIdentifierToken
    return (EIdentifier expr)

  parseEConstant = do
    expr <- parseConstantToken
    return (EConstant expr)

  parseEStringLiteral = do
    expr <- parseStringLiteralToken
    return (EStringLiteral expr)

  parseEParens = do
    parseToken (Token Nothing (OperatorToken (Operator "(")))
    expr <- parseEPrimary
    parseToken (Token Nothing (OperatorToken (Operator ")")))
    return (EParens expr)

  parseEPrimary =
      parseEIdentifier <|>
      parseEConstant <|>
      parseEStringLiteral<|>
      parseEParens

  parseLeftRecursion parseLeft parseRight a = do
    left <- parseLeft
    try (
      do
        right <- many1 (parseRight)
        return (a left right)) <|>
      return left

  parseBinaryOperator :: String -> ParsecT [Token] u Identity Expression -> (Expression -> [Expression] -> Expression) -> ParsecT [Token] u Identity Expression
  parseBinaryOperator a b c = do
    parseLeftRecursion parseLeft parseRight c
    where
      parseLeft = b
      parseRight = do
        parseToken (Token Nothing (OperatorToken (Operator a)))
        expr <- parseLeft
        return expr

  parseEArraySubscript =
    parseLeftRecursion parseLeft parseRight EArraySubscript
    where
      parseLeft = parseEFunctionCall
      parseRight = between (parseToken (Token Nothing (OperatorToken (Operator "[")))) (parseToken (Token Nothing (OperatorToken (Operator "]")))) parseEPrimary

  parseEFunctionCall = do
    parseLeftRecursion parseLeft parseRight EFunctionCall
    where
      parseLeft = parseEPrimary
      parseRight = between (parseToken (Token Nothing (OperatorToken (Operator "(")))) (parseToken (Token Nothing (OperatorToken (Operator ")")))) (optionMaybe parseEArgumentList)

  parseEStructOrUnionMember = do
    parseLeftRecursion parseLeft parseRight EStructOrUnionMember
    where
      parseLeft = parseEPrimary
      parseRight = do
        (parseToken (Token Nothing (OperatorToken (Operator "."))) <|> parseToken (Token Nothing (OperatorToken (Operator "->"))))
        expr <- parseLeft
        return expr

  parseEPostfixIncrement = do
    expr <- parseEPrimary
    parseToken (Token Nothing (OperatorToken (Operator "++")))
    return (EPostfixIncrement expr)

  parseEPostfixDecrement = do
    expr <- parseEPrimary
    parseToken (Token Nothing (OperatorToken (Operator "--")))
    return (EPostfixDecrement expr)

  parseEPostfix =
    try parseEArraySubscript <|>
    try parseEFunctionCall <|>
    try parseEStructOrUnionMember <|>
    try parseEPostfixIncrement <|>
    try parseEPostfixDecrement <|>
    parseEPrimary

  parseEArgumentList =
    do
      list <- sepBy1 parseEAssignment (parseToken (Token Nothing (OperatorToken (Operator ","))))
      return (EArgumentList list)
      <|>
    parseEAssignment

  parseEPrefixIncrement = do
    parseToken (Token Nothing (OperatorToken (Operator "++")))
    expr <- parseEPostfix
    return (EPrefixIncrement expr)

  parseEPrefixDecrement = do
    parseToken (Token Nothing (OperatorToken (Operator "--")))
    expr <- parseEPostfix
    return (EPrefixDecrement expr)

  parseEAddressOperator = do
    parseToken (Token Nothing (OperatorToken (Operator "&")))
    expr <- parseEPostfix
    return (EAddressOperator expr)

  parseEIndirectionOperator = do
    parseToken (Token Nothing (OperatorToken (Operator "*")))
    expr <- parseEPostfix
    return (EIndirectionOperator expr)

  parseEArithmeticOperator = do
    op <- parseToken (Token Nothing (OperatorToken (Operator "+"))) <|>
      parseToken (Token Nothing (OperatorToken (Operator "-"))) <|>
      parseToken (Token Nothing (OperatorToken (Operator "~"))) <|>
      parseToken (Token Nothing (OperatorToken (Operator "!")))
    expr <- parseEPostfix
    return (EArithmeticOperator ((tvOperator . tValue) op, expr))

  parseEUnary =
    try parseEPrefixIncrement <|>
    try parseEPrefixDecrement <|>
    try parseEAddressOperator <|>
    try parseEIndirectionOperator <|>
    try parseEArithmeticOperator <|>
    parseEPostfix

  parseECast = parseEUnary

  parseEProduct = parseBinaryOperator "*" parseECast EProduct

  parseEQuotient = parseBinaryOperator "/" parseECast EQuotient

  parseERemainder = parseBinaryOperator "%" parseECast ERemainder

  parseEMultiplicative =
    try parseEProduct <|>
    try parseEQuotient <|>
    try parseERemainder <|>
    parseECast

  parseEAddition = parseBinaryOperator "+" parseEMultiplicative EAddition

  parseESubtraction = parseBinaryOperator "-" parseEMultiplicative ESubtraction

  parseEAdditive =
    try parseEAddition <|>
    try parseESubtraction <|>
    parseEMultiplicative

  parseELeftShift = parseBinaryOperator "<<" parseEAdditive ELeftShift

  parseERightShift = parseBinaryOperator ">>" parseEAdditive ERightShift

  parseEShift =
    try parseELeftShift <|>
    try parseERightShift <|>
    parseEAdditive

  parseELesser = parseBinaryOperator "<" parseEShift ELesser

  parseEGreater = parseBinaryOperator ">" parseEShift EGreater

  parseELesserOrEqual = parseBinaryOperator "<=" parseEShift ELesserOrEqual

  parseEGreaterOrEqual = parseBinaryOperator ">=" parseEShift EGreaterOrEqual

  parseERelational =
    try parseELesser <|>
    try parseEGreater <|>
    try parseELesserOrEqual <|>
    try parseEGreaterOrEqual <|>
    parseEShift

  parseEEqual = parseBinaryOperator "==" parseERelational EEqual

  parseENotEqual = parseBinaryOperator "!=" parseERelational ENotEqual

  parseEquality =
    try parseEEqual <|>
    try parseENotEqual <|>
    parseERelational

  parseEBitwiseAnd = parseBinaryOperator "&" parseEquality EBitwiseAnd

  parseEBitwiseExclusiveOr = parseBinaryOperator "|" parseEBitwiseAnd EBitwiseExclusiveOr

  parseEBitwiseInclusiveOr = parseBinaryOperator "|" parseEBitwiseExclusiveOr EBitwiseInclusiveOr

  parseELogicalAnd = parseBinaryOperator "&&" parseEBitwiseInclusiveOr ELogicalAnd

  parseELogicalOr = parseBinaryOperator "||" parseELogicalAnd ELogicalOr

  parseEConditional = parseELogicalOr

  parseEAssignment =
    parseLeftRecursion parseLeft parseRight EAssignment
    where
      parseLeft =
        try (
          do
            left <- parseEConditional
            return left) <|>
          parseEUnary
      parseRight = do
        op <-
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
        expr <- parseLeft
        return ((tvOperator . tValue) op, expr)

  parseExpression = parseEAssignment

  parseDeclaration = do
    spec <- parseDSpecifiers
    dec <- optionMaybe parseDInitDeclaratorList
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    return (DDeclaration spec dec)

  parseDSpecifiers = do
    specifiers <-
      many1 (
        parseDStorageClassSpecifier <|>
        parseDTypeSpecifier <|>
        parseDTypeQualifier)
    return (DSpecifiers specifiers)

  parseDInitDeclaratorList =
    do
      list <- sepBy1 parseDInitDeclarator (parseToken (Token Nothing (OperatorToken (Operator ","))))
      return (DInitDeclaratorList list)
      <|>
    parseDDeclarator

  parseDInitDeclarator = 
    try (
      do
        dec <- parseDDeclarator
        parseToken (Token Nothing (OperatorToken (Operator "=")))
        expr <- parseExpression
        return (DInitDeclarator dec expr)) <|>
      parseDDeclarator

  parseDStorageClassSpecifier = do
    specifier <-
      parseToken (Token Nothing (KeywordToken (Keyword "typedef"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "extern"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "static"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "auto"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "register")))
    return (DStorageClassSpecifier ((tvKeyword . tValue) specifier))

  parseDTypeSpecifier = do
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
    return (DTypeSpecifier ((tvKeyword . tValue) specifier))

  parseDTypeQualifier = do
    qualifier <-
      parseToken (Token Nothing (KeywordToken (Keyword "const"))) <|>
      parseToken (Token Nothing (KeywordToken (Keyword "volatile")))
    return (DTypeQualifier ((tvKeyword . tValue) qualifier))

  parseDDeclarator = do
    pointer <- optionMaybe parseDPointer
    dec <- parseDDirectDeclarator
    return (DDeclarator pointer dec)

  parseDDirectDeclarator = parseDDirectDeclaratorFunctionCall

  parseDDirectDeclaratorIdentifier = do
    expr <- parseEIdentifier
    return (DDirectDeclaratorIdentifier expr)

  parseDDirectDeclaratorFunctionCall = do
    parseLeftRecursion parseLeft parseRight DDirectDeclaratorFunctionCall
    where
      parseLeft = parseDDirectDeclaratorIdentifier
      parseRight = between (parseToken (Token Nothing (OperatorToken (Operator "(")))) (parseToken (Token Nothing (OperatorToken (Operator ")")))) parseDParameterTypeList

  parseDPointer = do
    many1 (parseToken (Token Nothing (OperatorToken (Operator "*"))))
    list <- optionMaybe (parseDTypeQualifierList)
    return (DPointer list)

  parseDTypeQualifierList = do
    list <- many1 (parseDTypeQualifier)
    return (DTypeQualifierList list)

  parseDParameterTypeList = parseDParameterList

  parseDParameterList = do
    list <- many (parseDParameterDeclaration)
    return (DParameterList list)

  parseDParameterDeclaration = do
    spec <- parseDSpecifiers
    dec <- parseDDeclarator
    return (DParameterDeclaration spec dec)

  parseDIdentifierList = do
    list <- sepBy1 parseEIdentifier (parseToken (Token Nothing (OperatorToken (Operator ","))))
    return (DIdentifierList list)

  parseSLabeledIdentifier = do
    expr <- parseEIdentifier
    parseToken (Token Nothing (OperatorToken (Operator ":")))
    stat <- parseStatement
    return (SLabeledIdentifier expr stat)

  parseSLabeledCase = do
    parseToken (Token Nothing (KeywordToken (Keyword "case")))
    expr <- parseExpression
    parseToken (Token Nothing (OperatorToken (Operator ":")))
    statement <- parseStatement
    return (SLabeledCase expr statement)

  parseSLabeledDefault = do
    parseToken (Token Nothing (KeywordToken (Keyword "default")))
    parseToken (Token Nothing (OperatorToken (Operator ":")))
    statement <- parseStatement
    return (SLabeledDefault statement)

  parseSDeclarationList = do
    list <- many1 parseDeclaration
    return (SDeclarationList list)

  parseSList = do
    list <- many1 parseStatement
    return (SList list)

  parseSCompound = do
    parseToken (Token Nothing (PunctuatorToken (Punctuator "{")))
    firstList <- optionMaybe parseSDeclarationList
    secondList <- optionMaybe parseSList
    parseToken (Token Nothing (PunctuatorToken (Punctuator "}")))
    return (SCompound firstList secondList)

  parseSExpression = do
    expr <- optionMaybe parseExpression
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    return (SExpression expr)

  parseSIf = do
    parseToken (Token Nothing (KeywordToken (Keyword "if")))
    parseToken (Token Nothing (OperatorToken (Operator "(")))
    expr <- parseExpression
    parseToken (Token Nothing (OperatorToken (Operator ")")))
    statement <- parseStatement
    return (SIf expr statement)

  parseSIfElse = do
    parseToken (Token Nothing (KeywordToken (Keyword "if")))
    parseToken (Token Nothing (OperatorToken (Operator "(")))
    expr <- parseExpression
    parseToken (Token Nothing (OperatorToken (Operator ")")))
    firstStatement <- parseStatement
    parseToken (Token Nothing (KeywordToken (Keyword "else")))
    secondStatement <- parseStatement
    return (SIfElse expr firstStatement secondStatement)

  parseSSwitch = do
    parseToken (Token Nothing (KeywordToken (Keyword "switch")))
    parseToken (Token Nothing (OperatorToken (Operator "(")))
    expr <- parseExpression
    parseToken (Token Nothing (OperatorToken (Operator ")")))
    statement <- parseStatement
    return (SSwitch expr statement)

  parseSWhile = do
    parseToken (Token Nothing (KeywordToken (Keyword "while")))
    parseToken (Token Nothing (OperatorToken (Operator "(")))
    expr <- parseExpression
    parseToken (Token Nothing (OperatorToken (Operator ")")))
    statement <- parseStatement
    return (SWhile expr statement)

  parseSDo = do
    parseToken (Token Nothing (KeywordToken (Keyword "do")))
    statement <- parseStatement
    parseToken (Token Nothing (KeywordToken (Keyword "while")))
    parseToken (Token Nothing (OperatorToken (Operator "(")))
    expr <- parseExpression
    parseToken (Token Nothing (OperatorToken (Operator ")")))
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    return (SDo statement expr)

  parseSFor = do
    parseToken (Token Nothing (KeywordToken (Keyword "for")))
    parseToken (Token Nothing (OperatorToken (Operator "(")))
    firstExpr <- optionMaybe parseExpression
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    secondExpr <- optionMaybe parseExpression
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    thirdExpr <- optionMaybe parseExpression
    parseToken (Token Nothing (OperatorToken (Operator ")")))
    statement <- parseStatement
    return (SFor firstExpr secondExpr thirdExpr statement)

  parseSGoto = do
    parseToken (Token Nothing (KeywordToken (Keyword "goto")))
    identifier <- parseEIdentifier
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    return (SGoto identifier)

  parseSContinue = do
    parseToken (Token Nothing (KeywordToken (Keyword "continue")))
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    return SContinue

  parseSBreak = do
    parseToken (Token Nothing (KeywordToken (Keyword "break")))
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    return SBreak

  parseSReturn = do
    parseToken (Token Nothing (KeywordToken (Keyword "return")))
    expr <- optionMaybe parseExpression
    parseToken (Token Nothing (PunctuatorToken (Punctuator ";")))
    return (SReturn expr)

  parseStatement =
    try parseSLabeledIdentifier <|>
    parseSLabeledCase <|>
    parseSLabeledDefault <|>
    parseSCompound <|>
    parseSExpression <|>
    try parseSIfElse <|>
    parseSIf <|>
    parseSSwitch <|>
    parseSWhile <|>
    parseSDo <|>
    parseSFor <|>
    parseSGoto <|>
    parseSContinue <|>
    parseSBreak <|>
    parseSReturn

  parseEDTranslationUnit = do
    list <- many (try parseEDDeclaration <|> try parseEDFunction)
    return (EDTranslationUnit list)

  parseEDDeclaration = do
    dec <- parseDeclaration
    return (EDDeclaration dec)

  parseEDFunction = do
    firstDec <- optionMaybe parseDSpecifiers
    secondDec <- parseDDeclarator
    firstStat <- optionMaybe parseSDeclarationList
    secondStat <- parseSCompound
    return (EDFunction firstDec secondDec firstStat secondStat)

  parse = Text.Parsec.parse (parseEDTranslationUnit) ""
