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
    SelectorState carries state between selectors. A SelectorState carries a directed acyclic graphs (graph) which is a graph
    representation of the intermediate representation, an accumulator (counter) for the numbering of graph nodes, and the most
    recent side-effecting node (chain).
  -}
  data SelectorState = SelectorState {graphs :: [Graph], counter :: Integer, chain :: Integer}

  setGraph a (SelectorState _ b c) = SelectorState a b c

  setCounter a (SelectorState b c d) = SelectorState b (a c) d

  setChain a (SelectorState b c _) = SelectorState b c a

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
    let entryNode = Node (counter got) (EntryToken) [(Other, Nothing)] Nothing
    let newGraph = appendGraph [Graph [entryNode] []] (graphs got)
    let basicBlocks = execState (selectIRBasicBlocks d) ((setGraph newGraph . setCounter (+1)) got)
    put (SelectorState (graphs basicBlocks ++ [Graph [] []]) 0 0)
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
              Edge ((chain got)) ((counter got) + 5) 0,
              Edge ((counter got) + 3) ((counter got) + 5) 0,
              Edge ((counter got) + 4) ((counter got) + 5) 0]
        let newGraph = appendGraph [Graph newNodes newEdges] (graphs got)
        put ((setGraph newGraph . setCounter (+6) . setChain ((counter got) + 5)) got)

  selectIRModule :: IRModule -> SelectorStateMonad [Graph]
  selectIRModule (IRModule a) = do
    got <- get
    let newGraph = (execState (selectIRGlobalValues a) got)
    return ((init . graphs) newGraph)

  select :: IRModule -> [Graph]
  select a = evalState (selectIRModule a) (SelectorState ([Graph [] []]) 0 0)
