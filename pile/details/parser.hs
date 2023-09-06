module Parser where
  import Lexer
  import Data.Functor.Identity
  import Text.Parsec
  import Syntax

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

  parseTIdentifier =
    parseTokenValue testTok
    where
      testTok (Token pos (TIdentifier x)) = Just x
      testTok x = Nothing

  parseTConstant =
    parseTokenValue testTok
    where
      testTok (Token pos (TConstant x)) = Just x
      testTok x = Nothing

  parseTStringLiteral =
    parseTokenValue testTok
    where
      testTok (Token pos (TStringLiteral x)) = Just x
      testTok x = Nothing

  parseEIdentifier = do
    expr <- parseTIdentifier
    return (EIdentifier expr)

  parseEConstant = do
    expr <- parseTConstant
    return (EConstant expr)

  parseEStringLiteral = do
    expr <- parseTStringLiteral
    return (EStringLiteral expr)

  parseEParens = do
    parseToken (Token Nothing (TOperator (COperator "(")))
    expr <- parseEPrimary
    parseToken (Token Nothing (TOperator (COperator ")")))
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

  parseBinaryOperator :: String -> ParsecT [Token] u Identity CExpression -> (CExpression -> [CExpression] -> CExpression) -> ParsecT [Token] u Identity CExpression
  parseBinaryOperator a b c = do
    parseLeftRecursion parseLeft parseRight c
    where
      parseLeft = b
      parseRight = do
        parseToken (Token Nothing (TOperator (COperator a)))
        expr <- parseLeft
        return expr

  parseEArraySubscript =
    parseLeftRecursion parseLeft parseRight EArraySubscript
    where
      parseLeft = parseEFunctionCall
      parseRight = between (parseToken (Token Nothing (TOperator (COperator "[")))) (parseToken (Token Nothing (TOperator (COperator "]")))) parseEPrimary

  parseEFunctionCall = do
    parseLeftRecursion parseLeft parseRight EFunctionCall
    where
      parseLeft = parseEPrimary
      parseRight = between (parseToken (Token Nothing (TOperator (COperator "(")))) (parseToken (Token Nothing (TOperator (COperator ")")))) (optionMaybe parseEArgumentList)

  parseEStructOrUnionMember = do
    parseLeftRecursion parseLeft parseRight EStructOrUnionMember
    where
      parseLeft = parseEPrimary
      parseRight = do
        (parseToken (Token Nothing (TOperator (COperator "."))) <|> parseToken (Token Nothing (TOperator (COperator "->"))))
        expr <- parseLeft
        return expr

  parseEPostfixIncrement = do
    expr <- parseEPrimary
    parseToken (Token Nothing (TOperator (COperator "++")))
    return (EPostfixIncrement expr)

  parseEPostfixDecrement = do
    expr <- parseEPrimary
    parseToken (Token Nothing (TOperator (COperator "--")))
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
      list <- sepBy1 parseEAssignment (parseToken (Token Nothing (TOperator (COperator ","))))
      return (EArgumentList list)
      <|>
    parseEAssignment

  parseEPrefixIncrement = do
    parseToken (Token Nothing (TOperator (COperator "++")))
    expr <- parseEPostfix
    return (EPrefixIncrement expr)

  parseEPrefixDecrement = do
    parseToken (Token Nothing (TOperator (COperator "--")))
    expr <- parseEPostfix
    return (EPrefixDecrement expr)

  parseEAddressOperator = do
    parseToken (Token Nothing (TOperator (COperator "&")))
    expr <- parseEPostfix
    return (EAddressOperator expr)

  parseEIndirectionOperator = do
    parseToken (Token Nothing (TOperator (COperator "*")))
    expr <- parseEPostfix
    return (EIndirectionOperator expr)

  parseEArithmeticOperator = do
    op <- parseToken (Token Nothing (TOperator (COperator "+"))) <|>
      parseToken (Token Nothing (TOperator (COperator "-"))) <|>
      parseToken (Token Nothing (TOperator (COperator "~"))) <|>
      parseToken (Token Nothing (TOperator (COperator "!")))
    expr <- parseEPostfix
    return (EArithmeticOperator ((tCOperator . tValue) op, expr))

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
          parseToken (Token Nothing (TOperator (COperator "="))) <|>
          parseToken (Token Nothing (TOperator (COperator "*="))) <|>
          parseToken (Token Nothing (TOperator (COperator "/="))) <|>
          parseToken (Token Nothing (TOperator (COperator "%="))) <|>
          parseToken (Token Nothing (TOperator (COperator "+="))) <|>
          parseToken (Token Nothing (TOperator (COperator "-="))) <|>
          parseToken (Token Nothing (TOperator (COperator "<<="))) <|>
          parseToken (Token Nothing (TOperator (COperator ">>="))) <|>
          parseToken (Token Nothing (TOperator (COperator "&="))) <|>
          parseToken (Token Nothing (TOperator (COperator "^="))) <|>
          parseToken (Token Nothing (TOperator (COperator "|=")))
        expr <- parseLeft
        return ((tCOperator . tValue) op, expr)

  parseCExpression = parseEAssignment

  parseDeclaration = do
    spec <- parseDSpecifiers
    dec <- optionMaybe parseDInitDeclaratorList
    parseToken (Token Nothing (TPunctuator (CPunctuator ";")))
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
      list <- sepBy1 parseDInitDeclarator (parseToken (Token Nothing (TOperator (COperator ","))))
      return (DInitDeclaratorList list)
      <|>
    parseDDeclarator

  parseDInitDeclarator = 
    try (
      do
        dec <- parseDDeclarator
        parseToken (Token Nothing (TOperator (COperator "=")))
        expr <- parseCExpression
        return (DInitDeclarator dec expr)) <|>
      parseDDeclarator

  parseDStorageClassSpecifier = do
    specifier <-
      parseToken (Token Nothing (TKeyword (CKeyword "typedef"))) <|>
      parseToken (Token Nothing (TKeyword (CKeyword "extern"))) <|>
      parseToken (Token Nothing (TKeyword (CKeyword "static"))) <|>
      parseToken (Token Nothing (TKeyword (CKeyword "auto"))) <|>
      parseToken (Token Nothing (TKeyword (CKeyword "register")))
    return (DStorageClassSpecifier ((tCKeyword . tValue) specifier))

  parseDTypeSpecifier = do
    specifier <-
      parseToken (Token Nothing (TKeyword (CKeyword "void"))) <|>
      parseToken (Token Nothing (TKeyword (CKeyword "char"))) <|>
      parseToken (Token Nothing (TKeyword (CKeyword "short"))) <|>
      parseToken (Token Nothing (TKeyword (CKeyword "int"))) <|>
      parseToken (Token Nothing (TKeyword (CKeyword "long"))) <|>
      parseToken (Token Nothing (TKeyword (CKeyword "float"))) <|>
      parseToken (Token Nothing (TKeyword (CKeyword "double"))) <|>
      parseToken (Token Nothing (TKeyword (CKeyword "signed"))) <|>
      parseToken (Token Nothing (TKeyword (CKeyword "unsigned")))
    return (DTypeSpecifier ((tCKeyword . tValue) specifier))

  parseDTypeQualifier = do
    qualifier <-
      parseToken (Token Nothing (TKeyword (CKeyword "const"))) <|>
      parseToken (Token Nothing (TKeyword (CKeyword "volatile")))
    return (DTypeQualifier ((tCKeyword . tValue) qualifier))

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
      parseRight = between (parseToken (Token Nothing (TOperator (COperator "(")))) (parseToken (Token Nothing (TOperator (COperator ")")))) parseDParameterTypeList

  parseDPointer = do
    many1 (parseToken (Token Nothing (TOperator (COperator "*"))))
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
    list <- sepBy1 parseEIdentifier (parseToken (Token Nothing (TOperator (COperator ","))))
    return (DIdentifierList list)

  parseSLabeledIdentifier = do
    expr <- parseEIdentifier
    parseToken (Token Nothing (TOperator (COperator ":")))
    stat <- parseStatement
    return (SLabeledIdentifier expr stat)

  parseSLabeledCase = do
    parseToken (Token Nothing (TKeyword (CKeyword "case")))
    expr <- parseCExpression
    parseToken (Token Nothing (TOperator (COperator ":")))
    statement <- parseStatement
    return (SLabeledCase expr statement)

  parseSLabeledDefault = do
    parseToken (Token Nothing (TKeyword (CKeyword "default")))
    parseToken (Token Nothing (TOperator (COperator ":")))
    statement <- parseStatement
    return (SLabeledDefault statement)

  parseSDeclarationList = do
    list <- many1 parseDeclaration
    return (SDeclarationList list)

  parseSList = do
    list <- many1 parseStatement
    return (SList list)

  parseSCompound = do
    parseToken (Token Nothing (TPunctuator (CPunctuator "{")))
    firstList <- optionMaybe parseSDeclarationList
    secondList <- optionMaybe parseSList
    parseToken (Token Nothing (TPunctuator (CPunctuator "}")))
    return (SCompound firstList secondList)

  parseSCExpression = do
    expr <- optionMaybe parseCExpression
    parseToken (Token Nothing (TPunctuator (CPunctuator ";")))
    return (SCExpression expr)

  parseSIf = do
    parseToken (Token Nothing (TKeyword (CKeyword "if")))
    parseToken (Token Nothing (TOperator (COperator "(")))
    expr <- parseCExpression
    parseToken (Token Nothing (TOperator (COperator ")")))
    statement <- parseStatement
    return (SIf expr statement)

  parseSIfElse = do
    parseToken (Token Nothing (TKeyword (CKeyword "if")))
    parseToken (Token Nothing (TOperator (COperator "(")))
    expr <- parseCExpression
    parseToken (Token Nothing (TOperator (COperator ")")))
    firstStatement <- parseStatement
    parseToken (Token Nothing (TKeyword (CKeyword "else")))
    secondStatement <- parseStatement
    return (SIfElse expr firstStatement secondStatement)

  parseSSwitch = do
    parseToken (Token Nothing (TKeyword (CKeyword "switch")))
    parseToken (Token Nothing (TOperator (COperator "(")))
    expr <- parseCExpression
    parseToken (Token Nothing (TOperator (COperator ")")))
    statement <- parseStatement
    return (SSwitch expr statement)

  parseSWhile = do
    parseToken (Token Nothing (TKeyword (CKeyword "while")))
    parseToken (Token Nothing (TOperator (COperator "(")))
    expr <- parseCExpression
    parseToken (Token Nothing (TOperator (COperator ")")))
    statement <- parseStatement
    return (SWhile expr statement)

  parseSDo = do
    parseToken (Token Nothing (TKeyword (CKeyword "do")))
    statement <- parseStatement
    parseToken (Token Nothing (TKeyword (CKeyword "while")))
    parseToken (Token Nothing (TOperator (COperator "(")))
    expr <- parseCExpression
    parseToken (Token Nothing (TOperator (COperator ")")))
    parseToken (Token Nothing (TPunctuator (CPunctuator ";")))
    return (SDo statement expr)

  parseSFor = do
    parseToken (Token Nothing (TKeyword (CKeyword "for")))
    parseToken (Token Nothing (TOperator (COperator "(")))
    firstExpr <- optionMaybe parseCExpression
    parseToken (Token Nothing (TPunctuator (CPunctuator ";")))
    secondExpr <- optionMaybe parseCExpression
    parseToken (Token Nothing (TPunctuator (CPunctuator ";")))
    thirdExpr <- optionMaybe parseCExpression
    parseToken (Token Nothing (TOperator (COperator ")")))
    statement <- parseStatement
    return (SFor firstExpr secondExpr thirdExpr statement)

  parseSGoto = do
    parseToken (Token Nothing (TKeyword (CKeyword "goto")))
    identifier <- parseEIdentifier
    parseToken (Token Nothing (TPunctuator (CPunctuator ";")))
    return (SGoto identifier)

  parseSContinue = do
    parseToken (Token Nothing (TKeyword (CKeyword "continue")))
    parseToken (Token Nothing (TPunctuator (CPunctuator ";")))
    return SContinue

  parseSBreak = do
    parseToken (Token Nothing (TKeyword (CKeyword "break")))
    parseToken (Token Nothing (TPunctuator (CPunctuator ";")))
    return SBreak

  parseSReturn = do
    parseToken (Token Nothing (TKeyword (CKeyword "return")))
    expr <- optionMaybe parseCExpression
    parseToken (Token Nothing (TPunctuator (CPunctuator ";")))
    return (SReturn expr)

  parseStatement =
    try parseSLabeledIdentifier <|>
    parseSLabeledCase <|>
    parseSLabeledDefault <|>
    parseSCompound <|>
    parseSCExpression <|>
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
    list <- many1 (try parseEDDeclaration <|> try parseEDFunction)
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
