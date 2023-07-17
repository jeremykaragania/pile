module Lexer where
  import Text.Parsec
  import qualified Text.Parsec.Token as P
  import Text.Parsec.Language

  data Token value = Token TokenName value deriving (Show)

  data TokenName =
    Keyword |
    Identifier |
    Constant |
    StringLiteral |
    Operator |
    Punctuator deriving (Show)

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
    value <- P.reserved lexer
    return (Token Keyword value)

  identifier = do
    value <- P.identifier lexer
    return (Token Identifier value)

  floatingConstant = do
    value <- P.float lexer
    return (Token Constant value)

  integerConstant = do
    value <- P.integer lexer
    return (Token Constant value)

  characterConstant = do
    value <- P.charLiteral lexer
    return (Token Constant value)

  stringLiteral = do
    value <- P.stringLiteral lexer
    return (Token StringLiteral value)

  operator = do
    value <- P.operator lexer
    return (Token Operator value)

  punctuatorConstant = do
    value <- oneOf "*,:=;#"
    return (Token Punctuator value)
