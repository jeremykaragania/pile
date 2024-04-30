module Syntax where
  data CToken =
    CKeywordToken {keyword :: String} |
    CIdentifierToken {identifier :: String} |
    CConstantToken {constant :: CConstant} |
    CStringLiteralToken {stringLiteral :: String} |
    COperatorToken {operator :: String} |
    CPunctuatorToken {punctuator :: String} deriving (Show, Eq, Ord)

  data CConstant =
    CFloatingConstant Double (Maybe Char) |
    CIntegerConstant Integer (Maybe (Char, (Maybe Char))) |
    CCharacterConstant Char deriving (Show, Eq, Ord)

  data CExpression =
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
    CBinary String CExpression CExpression |
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
    CReturn (Maybe CExpression) deriving (Show, Eq)

  data CExternalDefinition =
    CExternalDeclaration CDeclaration |
    CFunction (Maybe CDeclaration) CDeclaration (Maybe CStatement) CStatement deriving (Show, Eq)

  data CTranslationUnit = CTranslationUnit [CExternalDefinition] deriving (Show, Eq)

  data IRIntegerType =
    IRSigned |
    IRUnsigned deriving (Show, Eq, Ord)

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
    IRStructure Bool [IRType] deriving (Show, Eq, Ord)

  data IRConstant =
    IRIntegerConstant Integer |
    IRFloatingConstant Double |
    IRNullPointerConstant |
    IRArrayConstant [(IRType, IRConstant)] |
    IRStructureConstant [(IRType, IRConstant)] deriving (Show, Eq)

  data IRInstruction =
    IRRet (Maybe IRValue) |
    IRBrConditional IRType IRValue IRValue IRValue |
    IRBrUnconditional IRValue |
    IRSwitch IRType IRLabel IRLabel [(IRType, IRConstant, IRLabel)] |
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
    IRAlloca IRType |
    IRLoad IRType IRValue |
    IRStore IRType IRValue IRLabel |
    IRIcmp IRICondition IRType IRValue IRValue |
    IRFcmp IRFCondition IRType IRValue IRValue |
    IRTrunc IRType IRValue IRType |
    IRZext IRType IRValue IRType |
    IRSext IRType IRValue IRType |
    IRFptrunc IRType IRValue IRType |
    IRFpext IRType IRValue IRType |
    IRFptoui IRType IRValue IRType |
    IRFptosi IRType IRValue IRType |
    IRUitofp IRType IRValue IRType |
    IRSitofp IRType IRValue IRType |
    IRPtrtoint IRType IRValue IRType |
    IRInttoptr IRType IRValue IRType |
    IRBitcast IRType IRValue IRType |
    IRAddrspacecast IRType IRValue IRType deriving (Show, Eq)

  data IRICondition =
    IRIEq |
    IRINe |
    IRIUgt |
    IRIUge |
    IRIUlt |
    IRIUle |
    IRISgt |
    IRISge |
    IRISlt |
    IRISle deriving (Show, Eq)

  data IRFCondition =
    IRFFalse |
    IRFOeq |
    IRFOgt |
    IRFOge |
    IRFOlt |
    IRFOle |
    IRFOne |
    IRFOrd |
    IRFUeq |
    IRFUgt |
    IRFUge |
    IRFUlt |
    IRFUle |
    IRFUne |
    IRFUno |
    IRFTrue deriving (Show, Eq)

  data IRBasicBlock = IRBasicBlock IRLabel [(Maybe IRLabel, IRInstruction)] deriving (Show, Eq)

  data IRLabel =
    IRLabelName String |
    IRLabelNumber Integer deriving (Show, Eq)

  data IRValue =
    IRConstantValue IRConstant |
    IRLabelValue IRLabel deriving (Show, Eq)

  data IRArgument = IRArgument IRType (Maybe String) deriving (Show, Eq)

  data IRGlobalValue =
    IRFunctionGlobal IRType String [IRArgument] [IRBasicBlock] |
    IRVariableGlobal String IRType IRConstant deriving (Show, Eq)

  data IRModule = IRModule [IRGlobalValue] deriving (Show, Eq)

  data ARMCondition =
    ARMEq |
    ARMNe |
    ARMCs |
    ARMCc |
    ARMMi |
    ARMPl |
    ARMVs |
    ARMVc |
    ARMHi |
    ARMLs |
    ARMGe |
    ARMLt |
    ARMGt |
    ARMLe |
    ARMAl deriving (Show, Eq)

  data ARMOpcode =
    ARMMov |
    ARMMvn |
    ARMMovt |
    ARMVmov |
    ARMAdd |
    ARMAdc |
    ARMSub |
    ARMSbc |
    ARMRsb |
    ARMRsc |
    ARMCmp |
    ARMMul |
    ARMMla |
    ARMUmull |
    ARMUmlal |
    ARMSmull |
    ARMSmlal |
    ARMCmn |
    ARMTst |
    ARMTeq |
    ARMAnd |
    ARMEor |
    ARMOrr |
    ARMBic |
    ARMB |
    ARMBl |
    ARMBx |
    ARMLdr |
    ARMLdrb |
    ARMStr |
    ARMStrb |
    ARMNop deriving (Show, Eq)
