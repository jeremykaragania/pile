module Syntax where

  data CToken =
    TKeyword {tCKeyword :: CKeyword} |
    TIdentifier {tCIdentifier :: CIdentifier} |
    TConstant {tConstant :: CConstant} |
    TStringLiteral {tCStringLiteral :: CStringLiteral} |
    TOperator {tCOperator :: COperator} |
    TPunctuator {tCPunctuator :: CPunctuator} deriving (Show, Eq)

  data CKeyword = CKeyword {keyword :: String} deriving (Show, Eq)

  data CIdentifier = CIdentifier {identifier :: String} deriving (Show, Eq)

  data CConstant =
    CFloatingConstant {floatingConstant :: Double} |
    CIntegerConstant {integerConstant :: Integer} |
    CCharacterConstant {characterConstant :: Char} deriving (Show, Eq)

  data CStringLiteral = CStringLiteral {stringLiteral :: String} deriving (Show, Eq)

  data COperator = COperator {operator :: String} deriving (Show, Eq)

  data CPunctuator = CPunctuator {punctuator :: String} deriving (Show, Eq)

  data CExpression =
    EIdentifier CIdentifier |
    EConstant CConstant |
    EStringLiteral CStringLiteral |
    EParens CExpression |
    EArraySubscript CExpression [CExpression] |
    EFunctionCall CExpression [(Maybe CExpression)] |
    EStructOrUnionMember CExpression [CExpression] |
    EPostfixIncrement CExpression |
    EPostfixDecrement CExpression |
    EArgumentList [CExpression] |
    EPrefixIncrement CExpression |
    EPrefixDecrement CExpression |
    EAddressOperator CExpression |
    EIndirectionOperator CExpression |
    EArithmeticOperator (COperator, CExpression) |
    ESizeofEUnary CExpression |
    ESizeofTypeName CDeclaration |
    ECast CDeclaration CExpression |
    EProduct CExpression [CExpression] |
    EQuotient CExpression [CExpression] |
    ERemainder CExpression [CExpression] |
    EAddition CExpression [CExpression] |
    ESubtraction CExpression [CExpression] |
    ELeftShift CExpression [CExpression] |
    ERightShift CExpression [CExpression] |
    ELesser CExpression [CExpression] |
    EGreater CExpression [CExpression] |
    ELesserOrEqual CExpression [CExpression] |
    EGreaterOrEqual CExpression [CExpression] |
    EEqual CExpression [CExpression] |
    ENotEqual CExpression [CExpression] |
    EBitwiseAnd CExpression [CExpression] |
    EBitwiseExclusiveOr CExpression [CExpression] |
    EBitwiseInclusiveOr CExpression [CExpression] |
    ELogicalAnd CExpression [CExpression] |
    ELogicalOr CExpression [CExpression] |
    EConditional CExpression CExpression CExpression |
    EAssignment CExpression [(COperator, CExpression)] deriving Show

  data CDeclaration =
    DDeclaration CDeclaration (Maybe CDeclaration) |
    DSpecifiers [CDeclaration] |
    DInitDeclaratorList {dList :: [CDeclaration]} |
    DInitDeclarator CDeclaration CExpression |
    DStorageClassSpecifier CKeyword |
    DTypeSpecifier CKeyword |
    DStructOrUnionSpecifierComplete (Maybe CIdentifier) CDeclaration |
    DStructOrUnionSpecifierIncomplete CIdentifier |
    DStructOrUnion CKeyword |
    DStructDeclarationList [CDeclaration] |
    DStructDeclaration CDeclaration CDeclaration |
    DSpecifierQualifierList [CDeclaration] |
    DStructDeclaratorList [CDeclaration] |
    DStructDeclarator (Maybe CDeclaration) CExpression |
    DEnumSpecifierComplete (Maybe CIdentifier) CDeclaration |
    DEnumSpecifierIncomplete CIdentifier |
    DEnumeratorList [CDeclaration] |
    DEnumerator CExpression CExpression |
    DTypeQualifier CKeyword |
    DDeclarator (Maybe CDeclaration) CDeclaration |
    DDirectDeclaratorIdentifier CExpression |
    DDirectDeclaratorParens CDeclaration |
    DDirectDeclaratorArraySubscript CDeclaration |
    DDirectDeclaratorFunctionCall CDeclaration [CDeclaration] |
    DPointer (Maybe CDeclaration) |
    DTypeQualifierList [CDeclaration] |
    DParameterTypeList [CDeclaration] |
    DParameterList [CDeclaration] |
    DParameterDeclaration CDeclaration CDeclaration |
    DParameterDeclarationAbstract CDeclaration (Maybe CDeclaration) |
    DIdentifierList [CExpression] |
    DTypeName CDeclaration (Maybe CDeclaration) |
    DAbstractDeclarator (Maybe CDeclaration) CDeclaration |
    DDirectAbstractDeclaratorParens CDeclaration |
    DDirectAbstractDeclaratorArraySubscript CDeclaration |
    DDirectAbstractDeclaratorFunctionCall CDeclaration |
    DTypedefName CExpression |
    DInitializer (Either CExpression CDeclaration) |
    DInitializerList [CDeclaration] deriving Show

  data CStatement =
    SLabeledIdentifier CExpression CStatement |
    SLabeledCase CExpression CStatement |
    SLabeledDefault CStatement |
    SDeclarationList [CDeclaration] |
    SList [CStatement] |
    SCompound (Maybe CStatement) (Maybe CStatement) |
    SCExpression (Maybe CExpression) |
    SIf CExpression CStatement |
    SIfElse CExpression CStatement CStatement |
    SSwitch CExpression CStatement |
    SWhile CExpression CStatement |
    SDo CStatement CExpression |
    SFor (Maybe CExpression) (Maybe CExpression) (Maybe CExpression) CStatement |
    SGoto CExpression |
    SContinue |
    SBreak |
    SReturn (Maybe CExpression) deriving Show

  data CExternalDefinition =
    EDTranslationUnit [CExternalDefinition] |
    EDDeclaration CDeclaration |
    EDFunction (Maybe CDeclaration) CDeclaration (Maybe CStatement) CStatement deriving Show
