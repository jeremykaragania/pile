module Syntax where
  data CToken =
    CKeywordToken {keyword :: String} |
    CIdentifierToken {identifier :: String} |
    CConstantToken {constant :: CConstant} |
    CStringLiteralToken {stringLiteral :: String} |
    COperatorToken {operator :: String} |
    CPunctuatorToken {punctuator :: String} deriving (Show, Eq, Ord)

  data CConstant =
    CFloatingConstant {floatingConstant :: Double} |
    CIntegerConstant {integerConstant :: Integer} |
    CCharacterConstant {characterConstant :: Char} deriving (Show, Eq, Ord)

  data CExpression =
    CAss CExpression CExpression |
    CIdentifier {token :: CToken} |
    CConstant {token :: CToken} |
    CStringLiteral {token :: CToken} |
    CParens CExpression |
    CArraySubscript CExpression [CExpression] |
    CFunctionCall CExpression [(Maybe CExpression)] |
    CStructOrUnionMember CExpression [CExpression] |
    CPostfixIncrement CExpression |
    CPostfixDecrement CExpression |
    CArgumentList [CExpression] |
    CPrefixIncrement CExpression |
    CPrefixDecrement CExpression |
    CAddressOperator CExpression |
    CIndirectionOperator CExpression |
    CArithmeticOperator (CToken, CExpression) |
    CSizeofEUnary CExpression |
    CSizeofTypeName CDeclaration |
    CCast CDeclaration CExpression |
    CProduct CExpression CExpression |
    CQuotient CExpression CExpression |
    CRemainder CExpression CExpression |
    CAddition CExpression CExpression |
    CSubtraction CExpression CExpression |
    CLeftShift CExpression CExpression |
    CRightShift CExpression CExpression |
    CLesser CExpression CExpression |
    CGreater CExpression CExpression |
    CLesserOrEqual CExpression CExpression |
    CGreaterOrEqual CExpression CExpression |
    CEqual CExpression CExpression |
    CNotEqual CExpression CExpression |
    CBitwiseAnd CExpression CExpression |
    CBitwiseExclusiveOr CExpression CExpression |
    CBitwiseInclusiveOr CExpression CExpression |
    CLogicalAnd CExpression CExpression |
    CLogicalOr CExpression CExpression |
    CConditional CExpression CExpression CExpression |
    CAssignment String CExpression CExpression |
    CExpression [CExpression] deriving (Show, Eq, Ord)

  data CDeclaration =
    CDeclaration CDeclaration (Maybe CDeclaration) |
    CSpecifiers [CDeclaration] |
    CInitDeclaratorList {dList :: [CDeclaration]} |
    CInitDeclarator CDeclaration CExpression |
    CStorageClassSpecifier CToken |
    CTypeSpecifier CToken |
    CStructOrUnionSpecifierComplete (Maybe CToken) CDeclaration |
    CStructOrUnionSpecifierIncomplete CToken |
    CStructOrUnion CToken |
    CStructDeclarationList [CDeclaration] |
    CStructDeclaration CDeclaration CDeclaration |
    CSpecifierQualifierList [CDeclaration] |
    CStructDeclaratorList [CDeclaration] |
    CStructDeclarator (Maybe CDeclaration) CExpression |
    CEnumSpecifierComplete (Maybe CToken) CDeclaration |
    CEnumSpecifierIncomplete CToken |
    CEnumeratorList [CDeclaration] |
    CEnumerator CExpression CExpression |
    CTypeQualifier CToken |
    CDeclarator (Maybe CDeclaration) CDeclaration |
    CDirectDeclaratorIdentifier CExpression |
    CDirectDeclaratorParens CDeclaration |
    CDirectDeclaratorArraySubscript CDeclaration |
    CDirectDeclaratorFunctionCall CDeclaration [CDeclaration] |
    CPointer (Maybe CDeclaration) |
    CTypeQualifierList [CDeclaration] |
    CParameterTypeList [CDeclaration] |
    CParameterList [CDeclaration] |
    CParameterDeclaration CDeclaration CDeclaration |
    CParameterDeclarationAbstract CDeclaration (Maybe CDeclaration) |
    CIdentifierList [CExpression] |
    CTypeName CDeclaration (Maybe CDeclaration) |
    CAbstractDeclarator (Maybe CDeclaration) CDeclaration |
    CDirectAbstractDeclaratorParens CDeclaration |
    CDirectAbstractDeclaratorArraySubscript CDeclaration |
    CDirectAbstractDeclaratorFunctionCall CDeclaration |
    CTypedefName CExpression |
    CInitializer (Either CExpression CDeclaration) |
    CInitializerList [CDeclaration] deriving (Show, Eq, Ord)

  data CStatement =
    CLabeledIdentifier CExpression CStatement |
    CLabeledCase CExpression CStatement |
    CLabeledDefault CStatement |
    CDeclarationList [CDeclaration] |
    CList [CStatement] |
    CCompound (Maybe CStatement) (Maybe CStatement) |
    CCExpression (Maybe CExpression) |
    CIf CExpression CStatement |
    CIfElse CExpression CStatement CStatement |
    CSwitch CExpression CStatement |
    CWhile CExpression CStatement |
    CDo CStatement CExpression |
    CFor (Maybe CExpression) (Maybe CExpression) (Maybe CExpression) CStatement |
    CGoto CExpression |
    CContinue |
    CBreak |
    CReturn (Maybe CExpression) deriving Show

  data CExternalDefinition =
    CTranslationUnit [CExternalDefinition] |
    CExternalDeclaration CDeclaration |
    CFunction (Maybe CDeclaration) CDeclaration (Maybe CStatement) CStatement deriving Show

  data IRIntegerType =
    IRSigned |
    IRUnsigned deriving (Show, Eq)

  data IRType =
    IRVoid |
    IRFunction Bool IRType [IRType] |
    IRShortInteger IRIntegerType |
    IRInteger IRIntegerType |
    IRLongInteger IRIntegerType |
    IRFloat |
    IRDouble |
    IRLongDouble |
    IRPointer IRType |
    IRLabel |
    IRArray Integer IRType |
    IRStructure Bool [IRType] deriving (Show, Eq)

  data IRConstant =
    IRIntegerConstant Integer |
    IRFloatingConstant Double |
    IRNullPointerConstant |
    IRArrayConstant [(IRType, IRConstant)] |
    IRStructureConstant [(IRType, IRConstant)] deriving Show

  data IRInstruction =
    IRRet (Maybe IRValue) |
    IRBrConditional IRValue String String |
    IRBRUnconditional String |
    IRSwitch IRValue String [(IRConstant, String)] |
    IRFneg IRValue |
    IRAdd IRType IRValue IRValue |
    IRFadd IRType IRValue IRValue |
    IRSub IRType IRValue IRValue |
    IRFsub IRType IRValue IRValue |
    IRMul IRType IRValue IRValue |
    IRFmul IRType IRValue IRValue |
    IRUdiv IRType IRValue IRValue |
    IRSdiv IRType IRValue IRValue |
    IRFdiv IRType IRValue IRValue |
    IRUrem IRType IRValue IRValue |
    IRSrem IRType IRValue IRValue |
    IRFrem IRType IRValue IRValue |
    IRShl IRType IRValue IRValue |
    IRLshr IRType IRValue IRValue |
    IRAshr IRType IRValue IRValue |
    IRAnd IRType IRValue IRValue |
    IROr IRType IRValue IRValue |
    IRXor IRType IRValue IRValue |
    IRAlloca IRType (Maybe Integer) (Maybe Integer) |
    IRLoad IRType IRValue (Maybe Integer) |
    IRStore IRType IRValue IRLabel (Maybe Integer) deriving Show

  data IRBasicBlock = IRBasicBlock String [(Maybe IRLabel, IRInstruction)] deriving Show

  data IRLabel =
    IRLabelName String |
    IRLabelNumber Integer deriving Show

  data IRValue =
    IRConstantValue IRConstant |
    IRLabelValue IRLabel deriving Show

  data IRArgument = IRArgument IRType (Maybe String) deriving Show

  data IRGlobalValue =
    IRFunctionGlobal IRType String [IRArgument] [IRBasicBlock] |
    IRVariableGlobal String IRType IRConstant deriving Show

  data IRModule = IRModule [IRGlobalValue] deriving Show
