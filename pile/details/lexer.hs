module Lexer where
  import Text.Parsec
  import qualified Text.Parsec.Token as P
  import Text.Parsec.Language
  import Syntax

  data Token = Token {tPosition :: (Maybe SourcePos), tValue :: CToken} deriving Show

  instance Eq Token
    where
      (Token _ lhsVal) == (Token _ rhsVal) = if lhsVal == rhsVal then True else False

  languageDef = emptyDef {
    P.commentStart = "/*",
    P.commentEnd = "*/",
    P.commentLine = "//",
    P.nestedComments = True,
    P.identStart = letter <|> char '_',
    P.identLetter = alphaNum <|> char '_',
    P.reservedNames = [
      "auto",
      "double",
      "int",
      "struct",
      "break",
      "else",
      "long",
      "switch",
      "case",
      "enum",
      "register",
      "typedef",
      "char",
      "extern",
      "return",
      "union",
      "const",
      "float",
      "short",
      "unsigned",
      "continue",
      "for",
      "signed",
      "void",
      "default",
      "goto",
      "sizeof",
      "volatile",
      "do",
      "if",
      "static",
      "while"],
    P.reservedOpNames = [
      "~",
      "||",
      "|=",
      "|",
      "sizeof",
      "^=",
      "^",
      "]",
      "[",
      "?",
      ">>=",
      ">>",
      ">=",
      ">",
      "==",
      "=",
      "<=",
      "<<=",
      "<<",
      "<",
      ":",
      "/=",
      "/",
      ".",
      "->",
      "-=",
      "--",
      "-",
      ",",
      "+=",
      "++",
      "+",
      "*=",
      "*",
      ")",
      "(",
      "&=",
      "&&",
      "&",
      "%=",
      "%",
      "##",
      "#",
      "!=",
      "!"],
    P.caseSensitive = True
  }

  lexer = P.makeTokenParser languageDef

  scanCKeyword = do
    sourcePos <- getPosition
    value <- choice (map try (map (P.symbol lexer) (P.reservedNames languageDef)))
    return (Token (Just sourcePos) (CKeywordToken value))

  scanCIdentifier = do
    sourcePos <- getPosition
    value <- try (P.identifier lexer)
    return (Token (Just sourcePos) (CIdentifierToken value))

  scanCFloatingConstant = do
    sourcePos <- getPosition
    value <- try fractionalConstant
    suffix <- optionMaybe (oneOf "flFL")
    return (Token (Just sourcePos) (CConstantToken (CFloatingConstant value suffix)))
    where
      fractionalConstant =
        do
          first <- many1 digit
          _ <- char '.'
          second <- many digit
          return (read ((value first) ++ "." ++ (value second)) :: Double)
          <|>
        do
          first <- many digit
          _ <- char '.'
          second <- many1 digit
          return (read ((value first) ++ "." ++ (value second)) :: Double)
        where
          value [] = "0"
          value a = a

  scanCIntegerConstant = do
    sourcePos <- getPosition
    value <- try (P.integer lexer)
    suffix <- optionMaybe ((integerSuffix unsignedSuffix longSuffix) <|> (integerSuffix longSuffix unsignedSuffix))
    return (Token (Just sourcePos) (CConstantToken (CIntegerConstant value suffix)))
    where
      integerSuffix a b = do
        firstSuffix <- a
        secondSuffix <- optionMaybe b
        return (firstSuffix, secondSuffix)
      unsignedSuffix = oneOf "uU"
      longSuffix = oneOf "lL"

  scanCCharacterConstant = do
    sourcePos <- getPosition
    value <- try (P.charLiteral lexer)
    return (Token (Just sourcePos) (CConstantToken (CCharacterConstant value)))

  scanCStringLiteral = do
    sourcePos <- getPosition
    value <- try (P.stringLiteral lexer)
    return (Token (Just sourcePos) (CStringLiteralToken value))

  scanCOperator = do
    sourcePos <- getPosition
    value <- choice (map try (map (P.symbol lexer) (P.reservedOpNames languageDef)))
    return (Token (Just sourcePos) (COperatorToken value))

  scanCPunctuator = do
    sourcePos <- getPosition
    value <- try (choice (map (P.symbol lexer) (["[", "]", "(", ")", "{", "}", "*", ",", ":", "=", ";", "...", "#"])))
    return (Token (Just sourcePos) (CPunctuatorToken value))

  scanToken =
    scanCKeyword <|>
    scanCIdentifier <|>
    scanCOperator <|>
    scanCPunctuator <|>
    scanCFloatingConstant <|>
    scanCIntegerConstant <|>
    scanCCharacterConstant <|>
    scanCStringLiteral

  scan = parse (many (P.whiteSpace lexer >> scanToken)) ""
