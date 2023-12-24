module Selector where
  import Control.Monad.State
  import Data.Int
  import Syntax

  data NodeType =
    EntryToken |
    BasicBlock |
    Register ARMRegister |
    Constant |
    Opcode ARMOpcode deriving (Show, Eq)

  data MachineValueType =
    Byte |
    Halfword |
    Word |
    Other deriving (Show, Eq)

  data NodeValue =
    IntegerValue Integer |
    FloatingValue Double |
    OtherValue deriving (Show, Eq)

  data Node = Node {nodeId :: Integer, nodeType :: NodeType, nodeValues :: [(MachineValueType, Maybe NodeValue)], nodeOrder :: Maybe Integer} deriving (Show, Eq)

  data Edge = Edge {fromNode :: Integer, toNode :: Integer, result :: Integer} deriving (Show, Eq)

  data Graph = Graph {nodes :: [Node], edges :: [Edge]} deriving (Show, Eq)

  appendGraph a b = init b ++ [append (last b) (head a)] ++ tail a
    where append a b = Graph (nodes a ++ nodes b) (edges a ++ edges b)

  {-
    SelectorState carries state between selectors. A SelectorState carries a directed acyclic graphs (graph) which is a
    graph representation of the intermediate representation, and an accumulator (counter) for the numbering of graph nodes.
  -}
  data SelectorState = SelectorState {graphs :: [Graph], counter :: Integer}

  setGraph a (SelectorState _ b) = SelectorState a b

  setCounter a (SelectorState b c) = SelectorState b (a c)

  type SelectorStateMonad = State SelectorState

  toMachineValueType (IRShortInteger _) = Halfword
  toMachineValueType (IRInteger _) = Word
  toMachineValueType (IRLongInteger _) = Word

  toNodeValue (IRConstantValue (IRIntegerConstant a)) = IntegerValue a
  toNodeValue (IRConstantValue (IRFloatingConstant a)) = FloatingValue a

  toBytes Byte = 1
  toBytes Halfword = 2
  toBytes Word = 4

  selectIRGlobalValues :: [IRGlobalValue] -> SelectorStateMonad ()
  selectIRGlobalValues [] = return ()

  selectIRGlobalValues (a:as) = do
    got <- get
    let newGraph = execState (selectIRGlobalValue a) got
    put (newGraph)
    selectIRGlobalValues as

  selectIRGlobalValue :: IRGlobalValue -> SelectorStateMonad ()
  selectIRGlobalValue (IRFunctionGlobal a b c d) = do
    got <- get
    let basicBlocks = execState (selectIRBasicBlocks d) got
    put (SelectorState (graphs basicBlocks ++ [Graph [] []]) 0)
    where
      selectIRBasicBlocks :: [IRBasicBlock] -> SelectorStateMonad ()
      selectIRBasicBlocks [] = return ()

      selectIRBasicBlocks (a:as) = do
        got <- get
        let basicBlock = execState (selectIRBasicBlock a) got
        put (basicBlock)
        selectIRBasicBlocks as

      selectIRBasicBlock :: IRBasicBlock -> SelectorStateMonad ()
      selectIRBasicBlock (IRBasicBlock a b) = do
        got <- get
        let labeledInstructions = execState (selectIRLabeledInstructions b) got
        put (labeledInstructions)

      selectIRLabeledInstructions :: [(Maybe IRLabel, IRInstruction)] -> SelectorStateMonad ()
      selectIRLabeledInstructions [] = return ()

      selectIRLabeledInstructions (a:as) = do
        got <- get
        let labeledInstruction = execState (selectIRLabeledInstruction a) got
        put (labeledInstruction)
        selectIRLabeledInstructions as

      selectIRLabeledInstruction :: (Maybe IRLabel, IRInstruction) -> SelectorStateMonad ()
      selectIRLabeledInstruction (Just (IRLabelNumber a), IRAlloca b) = do
        got <- get
        let machineValueType = toMachineValueType b
        let bytes = toBytes machineValueType
        let newNodes = [
              Node (counter got) (Register ARMR13) [(Word, Nothing)] Nothing,
              Node ((counter got) + 1) (Constant) [(machineValueType, Just (IntegerValue bytes))] Nothing,
              Node ((counter got) + 2) (Opcode ARMSub) [(Word, Nothing)] Nothing]
        let newEdges = [
              Edge (counter got) ((counter got) + 2) 0,
              Edge ((counter got) + 1) ((counter got) + 2) 0]
        let newGraph = appendGraph [Graph newNodes newEdges] (graphs got)
        put ((setGraph newGraph . setCounter (+3)) got)

      selectIRLabeledInstruction (Nothing, IRStore b c@(IRConstantValue _) (IRLabelNumber d)) = do
        got <- get
        let nodeValue = toNodeValue c
        let machineValueType = toMachineValueType b
        let bytes = toBytes machineValueType
        let newNodes = [
              Node (counter got) (Register ARMR0) [(Word, Nothing)] Nothing,
              Node ((counter got) + 1) (Constant) [(Word, Just nodeValue)] Nothing,
              Node ((counter got) + 2) (Opcode ARMMov) [(Word, Nothing)] Nothing,
              Node ((counter got) + 3) (Register ARMR13) [(Word, Nothing)] Nothing,
              Node ((counter got) + 4) (Constant) [(Word, Just (IntegerValue ((d - 1) * bytes)))] Nothing,
              Node ((counter got) + 5) (Opcode ARMStr) [(Word, Nothing)] Nothing]
        let newEdges = []
        let newEdges = [
              Edge (counter got) ((counter got) + 2) 0,
              Edge ((counter got) + 1) ((counter got) + 2) 0,
              Edge ((counter got) + 3) ((counter got) + 5) 0,
              Edge ((counter got) + 4) ((counter got) + 5) 0]
        let newGraph = appendGraph [Graph newNodes newEdges] (graphs got)
        put ((setGraph newGraph . setCounter (+6)) got)

  selectIRModule :: IRModule -> SelectorStateMonad [Graph]
  selectIRModule (IRModule a) = do
    got <- get
    let newGraph = (execState (selectIRGlobalValues a) got)
    return ((init . graphs) newGraph)

  select :: IRModule -> [Graph]
  select a = evalState (selectIRModule a) (SelectorState ([Graph [] []]) 0)
