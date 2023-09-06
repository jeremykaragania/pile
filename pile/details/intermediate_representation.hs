module IntermediateRepresentation where

  data IRType =
    IRVoid |
    IRFunction Bool IRType [IRType] |
    IRInteger Integer |
    IRFloat Double |
    IRDouble Double |
    IRLongDouble Double |
    IRPointer Integer |
    IRArray Integer IRType |
    IRStructure Bool [IRType] deriving Show

  data IRConstant = 
    IRIntegerConstant IRType |
    IRFloatingConstant IRType |
    IRNullPointerConstant |
    IRArrayConstant IRType |
    IRStructureConstant IRType deriving Show
    
  data IRInstruction =
    IRRet (Maybe IRArgument) |
    IRBrConditional IRArgument String String |
    IRBRUnconditional String |
    IRSwitch IRArgument String [(IRConstant, String)] |
    IRFneg IRArgument |
    IRAdd IRArgument IRArgument |
    IRFadd IRArgument IRArgument |
    IRSub IRArgument IRArgument |
    IRMul IRArgument IRArgument |
    IRFMul IRArgument IRArgument |
    IRUdiv IRArgument IRArgument |
    IRSdiv IRArgument IRArgument |
    IRFdiv IRArgument IRArgument |
    IRUrem IRArgument IRArgument |
    IRSrem IRArgument IRArgument |
    IRFrem IRArgument IRArgument |
    IRShl IRArgument IRArgument |
    IRLshr IRArgument IRArgument |
    IRAshr IRArgument IRArgument |
    IRAnd IRArgument IRArgument |
    IROr IRArgument IRArgument |
    IRXOr IRArgument IRArgument |
    IRAlloca IRType (Maybe IRArgument) (Maybe Integer) |
    IRLoad IRArgument (Maybe Integer) |
    IRStore IRArgument IRArgument (Maybe Integer) deriving Show

  data IRBasicBlock = IRBasicBlock [((Maybe String), IRInstruction)] deriving Show

  data IRArgument = IRArgument IRType (Maybe String) deriving Show

  data IRGlobalValue =
    IRFunctionGlobal IRType String [IRArgument] [IRBasicBlock] |
    IRVariableGlobal String IRType deriving Show

  data IRModule =
    IRModule [IRGlobalValue] deriving Show
