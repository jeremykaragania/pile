module Lexer where
  import Text.Parsec
  import qualified Text.Parsec.Token as P
  import Text.Parsec.Language

  data Token = Token (Maybe SourcePos) TokenValue deriving Show

  instance Eq Token 
    where 
      (Token lhsPos lhsVal) == (Token rhsPos rhsVal) = if lhsVal == rhsVal then True else False

  tokenPos (Token pos val) = pos

  tokenVal (Token pos val) = val

  data TokenValue =
    KeywordToken Keyword |
    IdentifierToken Identifier |
    ConstantToken Constant |
    StringLiteralToken StringLiteral |
    OperatorToken Operator |
    PunctuatorToken Punctuator deriving (Show, Eq)

  data Keyword = Keyword String deriving (Show, Eq)

  data Identifier = Identifier String deriving (Show, Eq)

  data Constant =
    FloatingConstant Double |
    IntegerConstant Integer |
    CharacterConstant Char deriving (Show, Eq)

  data StringLiteral = StringLiteral String deriving (Show, Eq)

  data Operator = Operator String deriving (Show, Eq)

  data Punctuator = Punctuator String deriving (Show, Eq)

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
