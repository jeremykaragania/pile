module Selector where
  import Control.Monad.State
  import Data.Int
  import Syntax

  data NodeType =
    EntryToken |
    BasicBlock |
    Register ARMRegister |
    Constant NodeValue |
    Opcode ARMOpcode deriving (Show, Eq)

  data NodeValueType =
    Word |
    Halfword |
    Byte |
    Other deriving (Show, Eq)

  data NodeValue =
    IntegerValue Integer |
    FloatingValue Double |
    OtherValue deriving (Show, Eq)

  data Node = Node {nodeId :: Integer, nodeType :: NodeType, nodeValueTypes :: [NodeValueType], nodeOrder :: Maybe Integer} deriving (Show, Eq)

  data Edge = Edge {fromNode :: Integer, toNode :: Integer, result :: Integer} deriving (Show, Eq)

  data Graph = Graph {nodes :: [Node], edges :: [Edge]} deriving (Show, Eq)

  data SelectorState = SelectorState {graph :: Graph, counter :: Integer}

  type SelectorStateMonad = State SelectorState

  toNodeValueType (IRShortInteger _) = Halfword
  toNodeValueType (IRInteger _) = Word
  toNodeValueType (IRLongInteger _) = Word

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
    put (basicBlocks)
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
        let newNodes = [Node (counter got) (Opcode ARMSub) [] Nothing, Node ((counter got) + 1) (Register ARMR13) [] Nothing, Node ((counter got) + 2) (Constant 0) [] Nothing]
        put (SelectorState (Graph (((nodes . graph) got) ++ newNodes) ((edges . graph) got)) ((counter got) + 3))

  selectIRModule :: IRModule -> SelectorStateMonad Graph
  selectIRModule (IRModule a) = do
    got <- get
    let newGraph = (execState (selectIRGlobalValues a) got)
    return (graph newGraph)

  select :: IRModule -> Graph
  select a = evalState (selectIRModule a) (SelectorState (Graph [] []) 0)
