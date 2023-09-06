module IntermediateRepresentation where

  data IRType =
    IRVoid |
    IRFunction IRType [IRType] |
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
    
  data IRInstruction = IRInstruction deriving Show

  data IRBasicBlock = IRBasicBlock [IRInstruction] deriving Show

  data IRArgument = IRArgument IRType (Maybe String) deriving Show

  data IRGlobalValue =
    IRFunctionGlobal IRType String [IRArgument] [IRBasicBlock] |
    IRVariableGlobal String IRType deriving Show

  data IRModule =
    IRModule [IRGlobalValue] deriving Show
