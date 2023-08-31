module Lexer where
  import Text.Parsec
  import qualified Text.Parsec.Token as P
  import Text.Parsec.Language

  data Token = Token {tPosition :: (Maybe SourcePos), tValue :: TokenValue} deriving Show

  instance Eq Token 
    where 
      (Token lhsPos lhsVal) == (Token rhsPos rhsVal) = if lhsVal == rhsVal then True else False

  data TokenValue =
    KeywordToken {tvKeyword :: Keyword} |
    IdentifierToken {tvIdentifier :: Identifier} |
    ConstantToken {tvConstant :: Constant} |
    StringLiteralToken {tvLiteral :: StringLiteral} |
    OperatorToken {tvOperator :: Operator} |
    PunctuatorToken {tvPunctuator :: Punctuator} deriving (Show, Eq)

  data Keyword = Keyword {keyword :: String} deriving (Show, Eq)

  data Identifier = Identifier {identifier :: String} deriving (Show, Eq)

  data Constant =
    FloatingConstant {floatingConstant :: Double} |
    IntegerConstant {integerConstant :: Integer} |
    CharacterConstant {characterConstant :: Char} deriving (Show, Eq)

  data StringLiteral = StringLiteral {stringLiteral :: String} deriving (Show, Eq)

  data Operator = Operator {operator :: String} deriving (Show, Eq)

  data Punctuator = Punctuator {punctuator :: String} deriving (Show, Eq)

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
      "while"
    ],
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
      "!"
    ],
    P.caseSensitive = True
  }

  lexer = P.makeTokenParser languageDef

  scanKeyword = do
    sourcePos <- getPosition
    value <- choice (map try (map (P.symbol lexer) (P.reservedNames languageDef)))
    return (Token (Just sourcePos) (KeywordToken (Keyword value)))

  scanIdentifier = do
    sourcePos <- getPosition
    value <- try (P.identifier lexer)
    return (Token (Just sourcePos) (IdentifierToken (Identifier value)))

  scanFloatingConstant = do
    sourcePos <- getPosition
    value <- try (P.float lexer)
    return (Token (Just sourcePos) (ConstantToken (FloatingConstant value)))

  scanIntegerConstant = do
    sourcePos <- getPosition
    value <- try (P.integer lexer)
    return (Token (Just sourcePos) (ConstantToken (IntegerConstant value)))

  scanCharacterConstant = do
    sourcePos <- getPosition
    value <- try (P.charLiteral lexer)
    return (Token (Just sourcePos) (ConstantToken (CharacterConstant value)))

  scanStringLiteral = do
    sourcePos <- getPosition
    value <- try (P.stringLiteral lexer)
    return (Token (Just sourcePos) (StringLiteralToken (StringLiteral value)))

  scanOperator = do
    sourcePos <- getPosition
    value <- choice (map try (map (P.symbol lexer) (P.reservedOpNames languageDef)))
    return (Token (Just sourcePos) (OperatorToken (Operator value)))

  scanPunctuator = do
    sourcePos <- getPosition
    value <- try (choice (map (P.symbol lexer) (["[", "]", "(", ")", "{", "}", "*", ",", ":", "=", ";", "...", "#"])))
    return (Token (Just sourcePos) (PunctuatorToken (Punctuator value)))

  scanToken =
    scanKeyword <|>
    scanIdentifier <|>
    scanOperator <|>
    scanPunctuator <|>
    scanFloatingConstant <|>
    scanIntegerConstant <|>
    scanCharacterConstant <|>
    scanStringLiteral

  scan = parse (many (P.whiteSpace lexer >> scanToken)) ""
