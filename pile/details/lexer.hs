module Lexer where
  import Text.Parsec
  import qualified Text.Parsec.Token as P
  import Text.Parsec.Language

  type Token = (SourcePos, TokenValue)

  data TokenValue =
    Keyword String |
    Identifier String |
    FloatingConstant Double |
    IntegerConstant Integer|
    CharacterConstant Char|
    StringLiteral String |
    Operator String |
    Punctuator String deriving Show

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
    return (sourcePos, Keyword value)

  scanIdentifier = do
    sourcePos <- getPosition
    value <- try (P.identifier lexer)
    return (sourcePos, Identifier value)

  scanFloatingConstant = do
    sourcePos <- getPosition
    value <- try (P.float lexer)
    return (sourcePos, FloatingConstant value)

  scanIntegerConstant = do
    sourcePos <- getPosition
    value <- try (P.integer lexer)
    return (sourcePos, IntegerConstant value)

  scanCharacterConstant = do
    sourcePos <- getPosition
    value <- try (P.charLiteral lexer)
    return (sourcePos, CharacterConstant value)

  scanStringLiteral = do
    sourcePos <- getPosition
    value <- try (P.stringLiteral lexer)
    return (sourcePos, StringLiteral value)

  scanOperator = do
    sourcePos <- getPosition
    value <- choice (map try (map (P.symbol lexer) (P.reservedOpNames languageDef)))
    return (sourcePos, Operator value)

  scanPunctuator = do
    sourcePos <- getPosition
    value <- try (choice (map (P.symbol lexer) (["[", "]", "(", ")", "{", "}", "*", ",", ":", "=", ";", "...", "#"])))
    return (sourcePos, Punctuator value)

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
