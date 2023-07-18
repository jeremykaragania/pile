module Lexer where
  import Text.Parsec
  import qualified Text.Parsec.Token as P
  import Text.Parsec.Language

  data Token =
    Keyword String |
    Identifier String |
    FloatingConstant Double |
    IntegerConstant Integer|
    CharacterConstant Char|
    StringLiteral String |
    Operator String |
    Punctuator String deriving (Show)

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

  keyword = do
    value <- choice (map try (map (P.symbol lexer) (P.reservedNames languageDef)))
    return (Keyword value)

  identifier = do
    value <- P.identifier lexer
    return (Identifier value)

  floatingConstant = do
    value <- P.float lexer
    return (FloatingConstant value)

  integerConstant = do
    value <- P.integer lexer
    return (IntegerConstant value)

  characterConstant = do
    value <- P.charLiteral lexer
    return (CharacterConstant value)

  stringLiteral = do
    value <- P.stringLiteral lexer
    return (StringLiteral value)

  operator = do
    value <- choice (map try (map (P.symbol lexer) (P.reservedOpNames languageDef)))
    return (Operator value)

  punctuator = do
    value <- try (choice (map (P.symbol lexer) (["[", "]", "(", ")", "{", "}", "*", ",", ":", "=", ";", "...", "#"])))
    return (Punctuator value)

  token =
    keyword <|>
    identifier <|>
    floatingConstant <|>
    integerConstant <|>
    characterConstant <|>
    stringLiteral <|>
    operator <|>
    punctuator

  scan = parse (many (P.whiteSpace lexer >> Lexer.token)) ""
