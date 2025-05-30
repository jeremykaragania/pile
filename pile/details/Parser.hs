module Parser where
  import Lexer
  import Data.Functor.Identity
  import Text.Parsec
  import Syntax

  parseToken t = tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos _ _ = pos
      testTok x = if x == t then Just x else Nothing

  parseCToken testTok = tokenPrim showTok nextPos testTok
    where
      showTok x = show x
      nextPos pos _ _ = pos

  parseCIdentifierToken = parseCToken testTok
    where
      testTok (Token _ (CIdentifierToken x)) = Just $ CIdentifierToken x
      testTok _ = Nothing

  parseCConstantToken = parseCToken testTok
    where
      testTok (Token _ (CConstantToken x)) = Just $ CConstantToken x
      testTok _ = Nothing

  parseCStringLiteralToken = parseCToken testTok
    where
      testTok (Token _ (CStringLiteralToken x)) = Just $ CStringLiteralToken x
      testTok _ = Nothing

  parseCIdentifier = do
    expr <- parseCIdentifierToken
    return $ CIdentifier expr

  parseCConstant = do
    expr <- parseCConstantToken
    return $ CConstant expr

  parseCStringLiteral = do
    expr <- parseCStringLiteralToken
    return $ CStringLiteral expr

  parseCParens = do
    parseToken $ Token Nothing $ COperatorToken "("
    expr <- parseCPrimary
    parseToken $ Token Nothing $ COperatorToken ")"
    return $ CParens expr

  parseCPrimary =
      parseCIdentifier <|>
      parseCConstant <|>
      parseCStringLiteral <|>
      parseCParens

  parseLeftRecursion parseLeft parseRight a = do
    left <- parseLeft
    try (
      do
        right <- many1 parseRight
        return $ a left right) <|>
      return left

  parseBinaryOperator :: String -> (CExpression -> CExpression -> CExpression) -> ParsecT [Token] u Identity (CExpression -> CExpression -> CExpression)
  parseBinaryOperator a b = do
    parseToken $ Token Nothing $ COperatorToken a
    return b

  parseCArraySubscript = parseLeftRecursion parseLeft parseRight CArraySubscript
    where
      parseLeft = parseCPrimary
      parseRight = between (parseToken $ Token Nothing $ COperatorToken "[") (parseToken $ Token Nothing $ COperatorToken "]") parseCExpression

  parseCFunctionCall = parseLeftRecursion parseLeft parseRight CFunctionCall
    where
      parseLeft = parseCPrimary
      parseRight = between (parseToken $ Token Nothing $ COperatorToken "(") (parseToken $ Token Nothing $ COperatorToken ")") $ optionMaybe parseCArgumentList

  parseCStructOrUnionMember = parseLeftRecursion parseLeft parseRight CStructOrUnionMember
    where
      parseLeft = parseCPrimary
      parseRight = do
        parseToken (Token Nothing $ COperatorToken ".") <|> parseToken (Token Nothing $ COperatorToken "->")
        expr <- parseLeft
        return expr

  parseCPostfixIncrement = do
    expr <- parseCPrimary
    parseToken $ Token Nothing $ COperatorToken "++"
    return $ CPostfixIncrement expr

  parseCPostfixDecrement = do
    expr <- parseCPrimary
    parseToken $ Token Nothing $ COperatorToken "--"
    return $ CPostfixDecrement expr

  parseCPostfix =
    try parseCArraySubscript <|>
    try parseCFunctionCall <|>
    try parseCStructOrUnionMember <|>
    try parseCPostfixIncrement <|>
    try parseCPostfixDecrement <|>
    parseCPrimary

  parseCArgumentList =
    do
      list <- sepBy1 parseCAssignment $ parseToken $ Token Nothing $ COperatorToken ","
      return $ CArgumentList list
      <|>
    parseCAssignment

  parseCPrefixIncrement = do
    parseToken $ Token Nothing $ COperatorToken "++"
    expr <- parseCPostfix
    return $ CPrefixIncrement expr

  parseCPrefixDecrement = do
    parseToken $ Token Nothing $ COperatorToken "--"
    expr <- parseCPostfix
    return $ CPrefixDecrement expr

  parseCAddressOperator = do
    parseToken $ Token Nothing $ COperatorToken "&"
    expr <- parseCPostfix
    return $ CAddressOperator expr

  parseCIndirectionOperator = do
    parseToken $ Token Nothing $ COperatorToken "*"
    expr <- parseCPostfix
    return $ CIndirectionOperator expr

  parseCArithmeticOperator = do
    op <- parseToken (Token Nothing $ COperatorToken "+") <|>
      parseToken (Token Nothing $ COperatorToken "-") <|>
      parseToken (Token Nothing $ COperatorToken "~") <|>
      parseToken (Token Nothing $ COperatorToken "!")
    expr <- parseCPostfix
    return $ CArithmeticOperator (tValue op, expr)

  parseCUnary =
    try parseCPrefixIncrement <|>
    try parseCPrefixDecrement <|>
    try parseCAddressOperator <|>
    try parseCIndirectionOperator <|>
    try parseCArithmeticOperator <|>
    parseCPostfix

  parseCCast = parseCUnary

  parseCProduct = parseBinaryOperator "*" $ CBinary "*"

  parseCQuotient = parseBinaryOperator "/" $ CBinary "/"

  parseCRemainder = parseBinaryOperator "%" $ CBinary "%"

  parseCMultiplicative = chainl1 parseCCast op
    where
      op =
        parseCProduct <|>
        parseCQuotient <|>
        parseCRemainder

  parseCAddition = parseBinaryOperator "+" $ CBinary "+"

  parseCSubtraction = parseBinaryOperator "-" $ CBinary "-"

  parseCAdditive = chainl1 parseCMultiplicative op
    where
      op =
        parseCAddition <|>
        parseCSubtraction

  parseCLeftShift = parseBinaryOperator "<<" $ CBinary "<<"

  parseCRightShift = parseBinaryOperator ">>" $ CBinary ">>"

  parseCShift = chainl1 parseCAdditive op
    where
      op =
        parseCLeftShift <|>
        parseCRightShift

  parseCLesser = parseBinaryOperator "<" $ CBinary "<"

  parseCGreater = parseBinaryOperator ">" $ CBinary ">"

  parseCLesserOrEqual = parseBinaryOperator "<=" $ CBinary "<="

  parseCGreaterOrEqual = parseBinaryOperator ">=" $ CBinary ">="

  parseCRelational = chainl1 parseCShift op
    where
      op =
        parseCLesser <|>
        parseCGreater <|>
        parseCLesserOrEqual <|>
        parseCGreaterOrEqual

  parseCEqual = parseBinaryOperator "==" $ CBinary "=="

  parseCNotEqual = parseBinaryOperator "!=" $ CBinary "!="

  parseCEquality = chainl1 parseCRelational op
    where
      op =
        parseCEqual <|>
        parseCNotEqual

  parseCBitwiseAnd = chainl1 parseCEquality $ parseBinaryOperator "&" $ CBinary "&"

  parseCBitwiseExclusiveOr = chainl1 parseCBitwiseAnd $ parseBinaryOperator "^" $ CBinary "^"

  parseCBitwiseInclusiveOr = chainl1 parseCBitwiseExclusiveOr $ parseBinaryOperator "|" $ CBinary "|"

  parseCLogicalAnd = chainl1 parseCBitwiseInclusiveOr $ parseBinaryOperator "&&" $ CBinary "&&"

  parseCLogicalOr = chainl1 parseCLogicalAnd $ parseBinaryOperator "||" $ CBinary "||"

  parseCConditional = parseCLogicalOr

  parseCAssignment = chainr1 parseCConditional parseRight
    where
      parseRight = do
        op <-
          parseToken (Token Nothing $ COperatorToken "=") <|>
          parseToken (Token Nothing $ COperatorToken "*=") <|>
          parseToken (Token Nothing $ COperatorToken "/=") <|>
          parseToken (Token Nothing $ COperatorToken "%=") <|>
          parseToken (Token Nothing $ COperatorToken "+=") <|>
          parseToken (Token Nothing $ COperatorToken "-=") <|>
          parseToken (Token Nothing $ COperatorToken "<<=") <|>
          parseToken (Token Nothing $ COperatorToken ">>=") <|>
          parseToken (Token Nothing $ COperatorToken "&=") <|>
          parseToken (Token Nothing $ COperatorToken "^=") <|>
          parseToken (Token Nothing $ COperatorToken "|=")
        return $ CAssignment $ operator $ tValue op

  parseCExpression = do
      list <- sepBy1 parseCAssignment $ parseToken $ Token Nothing $ COperatorToken ","
      return $ CExpression list

  parseCDeclaration = do
    spec <- parseCSpecifiers
    dec <- optionMaybe parseCInitDeclaratorList
    parseToken $ Token Nothing $ CPunctuatorToken ";"
    return $ CDeclaration spec dec

  parseCSpecifiers = do
    specifiers <-
      many1 $
        parseCStorageClassSpecifier <|>
        parseCTypeCpecifier <|>
        parseCTypeQualifier
    return $ CSpecifiers specifiers

  parseCInitDeclaratorList =
    do
      list <- sepBy1 parseCInitDeclarator $ parseToken $ Token Nothing $ COperatorToken ","
      return $ CInitDeclaratorList list
      <|>
    parseCDeclarator

  parseCInitDeclarator =
    try (
      do
        dec <- parseCDeclarator
        parseToken $ Token Nothing $ COperatorToken "="
        expr <- parseCAssignment
        return $ CInitDeclarator dec expr) <|>
      parseCDeclarator

  parseCStorageClassSpecifier = do
    specifier <-
      parseToken (Token Nothing $ CKeywordToken "typedef") <|>
      parseToken (Token Nothing $ CKeywordToken "extern") <|>
      parseToken (Token Nothing $ CKeywordToken "static") <|>
      parseToken (Token Nothing $ CKeywordToken "auto") <|>
      parseToken (Token Nothing $ CKeywordToken "register")
    return $ CStorageClassSpecifier $ tValue specifier

  parseCTypeCpecifier = do
    specifier <-
      parseToken (Token Nothing $ CKeywordToken "void") <|>
      parseToken (Token Nothing $ CKeywordToken "char") <|>
      parseToken (Token Nothing $ CKeywordToken "short") <|>
      parseToken (Token Nothing $ CKeywordToken "int") <|>
      parseToken (Token Nothing $ CKeywordToken "long") <|>
      parseToken (Token Nothing $ CKeywordToken "float") <|>
      parseToken (Token Nothing $ CKeywordToken "double") <|>
      parseToken (Token Nothing $ CKeywordToken "signed") <|>
      parseToken (Token Nothing $ CKeywordToken "unsigned")
    return $ CTypeSpecifier $ tValue specifier

  parseCTypeQualifier = do
    qualifier <-
      parseToken (Token Nothing $ CKeywordToken "const") <|>
      parseToken (Token Nothing $ CKeywordToken "volatile")
    return $ CTypeQualifier $ tValue qualifier

  parseCDeclarator = do
    pointer <- optionMaybe parseCPointer
    dec <- parseCDirectDeclarator
    return $ CDeclarator pointer dec

  parseCDirectDeclarator = parseCDirectDeclaratorFunctionCall

  parseCDirectDeclaratorIdentifier = do
    expr <- parseCIdentifier
    return $ CDirectDeclaratorIdentifier expr

  parseCDirectDeclaratorFunctionCall = parseLeftRecursion parseLeft parseRight CDirectDeclaratorFunctionCall
    where
      parseLeft = parseCDirectDeclaratorIdentifier
      parseRight = between (parseToken $ Token Nothing $ COperatorToken "(") (parseToken $ Token Nothing $ COperatorToken ")") parseCParameterTypeList

  parseCPointer = do
    many1 $ parseToken $ Token Nothing $ COperatorToken "*"
    list <- optionMaybe $ parseCTypeQualifierList
    return $ CPointer list

  parseCTypeQualifierList = do
    list <- many1 parseCTypeQualifier
    return $ CTypeQualifierList list

  parseCParameterTypeList = parseCParameterList

  parseCParameterList = do
    list <- sepBy parseCParameterDeclaration $ parseToken $ Token Nothing $ COperatorToken ","
    return $ CParameterList list

  parseCParameterDeclaration = do
    spec <- parseCSpecifiers
    dec <- parseCDeclarator
    return $ CParameterDeclaration spec dec

  parseCIdentifierList = do
    list <- sepBy1 parseCIdentifier $ parseToken $ Token Nothing $ COperatorToken ","
    return $ CIdentifierList list

  parseCLabeledIdentifier = do
    expr <- parseCIdentifier
    parseToken $ Token Nothing $ COperatorToken ":"
    stat <- parseCStatement
    return $ CLabeledIdentifier expr stat

  parseCLabeledCase = do
    parseToken $ Token Nothing $ CKeywordToken "case"
    expr <- parseCConditional
    parseToken $ Token Nothing $ COperatorToken ":"
    statement <- parseCStatement
    return $ CLabeledCase expr statement

  parseCLabeledDefault = do
    parseToken $ Token Nothing $ CKeywordToken "default"
    parseToken $ Token Nothing $ COperatorToken ":"
    statement <- parseCStatement
    return $ CLabeledDefault statement

  parseCDeclarationList = do
    list <- many1 parseCDeclaration
    return $ CDeclarationList list

  parseCList = do
    list <- many1 parseCStatement
    return $ CList list

  parseCCompound = do
    parseToken $ Token Nothing $ CPunctuatorToken "{"
    firstList <- optionMaybe parseCDeclarationList
    secondList <- optionMaybe parseCList
    parseToken $ Token Nothing $ CPunctuatorToken "}"
    return $ CCompound firstList secondList

  parseCCExpression = do
    expr <- optionMaybe parseCExpression
    parseToken $ Token Nothing $ CPunctuatorToken ";"
    return $ CCExpression expr

  parseCIf = do
    parseToken $ Token Nothing $ CKeywordToken "if"
    parseToken $ Token Nothing $ COperatorToken "("
    expr <- parseCExpression
    parseToken $ Token Nothing $ COperatorToken ")"
    statement <- parseCStatement
    return $ CIf expr statement

  parseCIfClse = do
    parseToken $ Token Nothing $ CKeywordToken "if"
    parseToken $ Token Nothing $ COperatorToken "("
    expr <- parseCExpression
    parseToken $ Token Nothing $ COperatorToken ")"
    firstCStatement <- parseCStatement
    parseToken $ Token Nothing $ CKeywordToken "else"
    secondCStatement <- parseCStatement
    return $ CIfElse expr firstCStatement secondCStatement

  parseCSwitch = do
    parseToken $ Token Nothing $ CKeywordToken "switch"
    parseToken $ Token Nothing $ COperatorToken "("
    expr <- parseCExpression
    parseToken $ Token Nothing $ COperatorToken ")"
    statement <- parseCStatement
    return $ CSwitch expr statement

  parseCWhile = do
    parseToken $ Token Nothing $ CKeywordToken "while"
    parseToken $ Token Nothing $ COperatorToken "("
    expr <- parseCExpression
    parseToken $ Token Nothing $ COperatorToken ")"
    statement <- parseCStatement
    return $ CWhile expr statement

  parseCDo = do
    parseToken $ Token Nothing $ CKeywordToken "do"
    statement <- parseCStatement
    parseToken $ Token Nothing $ CKeywordToken "while"
    parseToken $ Token Nothing $ COperatorToken "("
    expr <- parseCExpression
    parseToken $ Token Nothing $ COperatorToken ")"
    parseToken $ Token Nothing $ CPunctuatorToken ";"
    return $ CDo statement expr

  parseCFor = do
    parseToken $ Token Nothing $ CKeywordToken "for"
    parseToken $ Token Nothing $ COperatorToken "("
    firstCxpr <- optionMaybe parseCExpression
    parseToken $ Token Nothing $ CPunctuatorToken ";"
    secondCxpr <- optionMaybe parseCExpression
    parseToken $ Token Nothing $ CPunctuatorToken ";"
    thirdCxpr <- optionMaybe parseCExpression
    parseToken $ Token Nothing $ COperatorToken ")"
    statement <- parseCStatement
    return $ CFor firstCxpr secondCxpr thirdCxpr statement

  parseCGoto = do
    parseToken $ Token Nothing $ CKeywordToken "goto"
    ident <- parseCIdentifier
    parseToken $ Token Nothing $ CPunctuatorToken ";"
    return $ CGoto ident

  parseCContinue = do
    parseToken $ Token Nothing $ CKeywordToken "continue"
    parseToken $ Token Nothing $ CPunctuatorToken ";"
    return CContinue

  parseCBreak = do
    parseToken $ Token Nothing $ CKeywordToken "break"
    parseToken $ Token Nothing $ CPunctuatorToken ";"
    return CBreak

  parseCReturn = do
    parseToken $ Token Nothing $ CKeywordToken "return"
    expr <- optionMaybe parseCExpression
    parseToken $ Token Nothing $ CPunctuatorToken ";"
    return $ CReturn expr

  parseCStatement =
    try parseCLabeledIdentifier <|>
    parseCLabeledCase <|>
    parseCLabeledDefault <|>
    parseCCompound <|>
    parseCCExpression <|>
    try parseCIfClse <|>
    parseCIf <|>
    parseCSwitch <|>
    parseCWhile <|>
    parseCDo <|>
    parseCFor <|>
    parseCGoto <|>
    parseCContinue <|>
    parseCBreak <|>
    parseCReturn

  parseCTranslationUnit = do
    list <- many1 $ try parseCExternalDeclaration <|> parseCFunction
    return $ CTranslationUnit list

  parseCExternalDeclaration = do
    dec <- parseCDeclaration
    return $ CExternalDeclaration dec

  parseCFunction = do
    firstDec <- optionMaybe parseCSpecifiers
    secondDec <- parseCDeclarator
    firstStat <- optionMaybe parseCDeclarationList
    secondStat <- parseCCompound
    return $ CFunction firstDec secondDec firstStat secondStat

  parse = Text.Parsec.parse parseCTranslationUnit ""
