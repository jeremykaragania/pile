module Lexer where
  import Text.Parsec
  import qualified Text.Parsec.Token as P
  import Text.Parsec.Language

  data Token = Token SourcePos TokenValue deriving (Show, Eq)

  pos (Token pos val) = pos

  val (Token pos val) = val

  data TokenValue =
    Keyword String |
    Identifier String |
    FloatingConstant Double |
    IntegerConstant Integer|
    CharacterConstant Char|
    StringLiteral String |
    Operator String |
    Punctuator String deriving (Show, Eq)

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
      "[",
      "]",
      "(",
      ")",
      ".",
      "->",
      "++",
      "--",
      "&",
      "*",
      "+",
      "-",
      "~",
      "!",
      "sizeof",
      "/",
      "%",
      "<<",
      ">>",
      "<",
      ">",
      "<=",
      ">=",
      "==",
      "!=",
      "^",
      "|",
      "&&",
      "||",
      "?",
      ":",
      "=",
      "*=",
      "/=",
      "%=",
      "+=",
      "-=",
      "<<=",
      ">>=",
      "&=",
      "^=",
      "|=",
      ",",
      "#",
      "##"
    ],
    P.caseSensitive = True
  }

  lexer = P.makeTokenParser languageDef

  scanKeyword = do
    sourcePos <- getPosition
    value <- choice (map try (map (P.symbol lexer) (P.reservedNames languageDef)))
    return (Token sourcePos (Keyword value))

  scanIdentifier = do
    sourcePos <- getPosition
    value <- try (P.identifier lexer)
    return (Token sourcePos (Identifier value))

  scanFloatingConstant = do
    sourcePos <- getPosition
    value <- try (P.float lexer)
    return (Token sourcePos (FloatingConstant value))

  scanIntegerConstant = do
    sourcePos <- getPosition
    value <- try (P.integer lexer)
    return (Token sourcePos (IntegerConstant value))

  scanCharacterConstant = do
    sourcePos <- getPosition
    value <- try (P.charLiteral lexer)
    return (Token sourcePos (CharacterConstant value))

  scanStringLiteral = do
    sourcePos <- getPosition
    value <- try (P.stringLiteral lexer)
    return (Token sourcePos (StringLiteral value))

  scanOperator = do
    sourcePos <- getPosition
    value <- choice (map try (map (P.symbol lexer) (P.reservedOpNames languageDef)))
    return (Token sourcePos (Operator value))

  scanPunctuator = do
    sourcePos <- getPosition
    value <- try (choice (map (P.symbol lexer) (["[", "]", "(", ")", "{", "}", "*", ",", ":", "=", ";", "...", "#"])))
    return (Token sourcePos (Punctuator value))

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
