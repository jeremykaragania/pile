module Selector where
  import Control.Monad.State
  import Data.Int
  import Syntax

  data OpcodeCondition = OpcodeCondition {opcode :: ARMOpcode, condition :: (Maybe ARMCondition)} deriving (Show, Eq)

  data NodeType =
    EntryToken |
    BasicBlock String |
    Register |
    Constant |
    Opcode OpcodeCondition deriving (Show, Eq)

  data MachineValueType =
    Byte |
    Halfword |
    Word |
    Other deriving (Show, Eq)

  data NodeValue =
    IntegerValue Integer |
    FloatingValue Double |
    OtherValue deriving (Show, Eq)

  data Node = Node {nodeID :: Integer, nodeType :: NodeType, nodeValues :: [(MachineValueType, Maybe NodeValue)]} deriving (Show, Eq)

  data Edge = Edge {fromNode :: Integer, toNode :: Integer, result :: Integer} deriving (Show, Eq)

  data Graph = Graph {nodes :: [Node], edges :: [Edge]} deriving (Show, Eq)

  appendGraph a b = init b ++ [append (last b) (head a)] ++ tail a
    where append a b = Graph (nodes a ++ nodes b) (edges a ++ edges b)

  {-
    SelectorState carries state between selectors. A SelectorState carries a directed acyclic graphs (graph) which is a graph
    representation of the intermediate representation, an accumulator (counter) for the numbering of graph nodes, the most
    recent side-effecting node (chain), the stack pointer offset (offset), and the global value name (global).
  -}
  data SelectorState = SelectorState {graphs :: [Graph], counter :: Integer, chain :: Integer, offset :: Integer, global :: String}

  setGraph a (SelectorState _ b c d e) = SelectorState a b c d e

  setCounter a (SelectorState b c d e f) = SelectorState b (a c) d e f

  setChain a (SelectorState b c _ d e) = SelectorState b c a d e

  setOffset a (SelectorState b c d e f) = SelectorState b c d (a e) f

  setGlobal a (SelectorState b c d e _) = SelectorState b c d e a

  type SelectorStateMonad = State SelectorState

  toMachineValueType (IRShortInteger _) = Halfword
  toMachineValueType (IRInteger _) = Word
  toMachineValueType (IRLongInteger _) = Word

  toNodeValue (IRConstantValue (IRIntegerConstant a)) = IntegerValue a
  toNodeValue (IRConstantValue (IRFloatingConstant a)) = FloatingValue a

  toBytes Byte = 1
  toBytes Halfword = 2
  toBytes Word = 4

  toOffset a b = (a - 1) * b

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
    let newNodes = [
          Node (counter got) (EntryToken) [(Other, Nothing)],
          Node (counter got + 1) (BasicBlock b) [(Other, Nothing)]]
    let newGraph = appendGraph [Graph newNodes []] (graphs got)
    let basicBlocks = execState (selectIRBasicBlocks d) ((setGraph newGraph . setCounter (+2) . setGlobal b) got)
    put (SelectorState (graphs basicBlocks ++ [Graph [] []]) 0 0 0 "")
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
        let basicBlock = Node (counter got) (BasicBlock ((global got) ++ "L" ++ label a)) [(Other, Nothing)]
        let newGraph = appendGraph [Graph [basicBlock] []] (graphs got)
        let labeledInstructions = execState (selectIRLabeledInstructions b) ((setGraph newGraph . setCounter (+1)) got)
        let newNodes = [
              Node (counter labeledInstructions) Register [(Word, Just (IntegerValue 13))],
              Node (counter labeledInstructions + 1) Constant [(Word, Just (IntegerValue (offset labeledInstructions)))],
              Node (counter labeledInstructions + 2) (Opcode (OpcodeCondition ARMAdd Nothing)) [(Word, Nothing)]]
        let newEdges = [
              Edge (counter labeledInstructions) (counter labeledInstructions + 2) 0,
              Edge (counter labeledInstructions) (counter labeledInstructions + 2) 0,
              Edge (counter labeledInstructions + 1) (counter labeledInstructions + 2) 0]
        let newGraph = appendGraph [Graph newNodes newEdges] (graphs labeledInstructions)
        put ((setGraph newGraph . setCounter (+3)) labeledInstructions)
        where
          label (IRLabelName a) = a
          label (IRLabelNumber a) = show a

      selectIRLabeledInstructions :: [(Maybe IRLabel, IRInstruction)] -> SelectorStateMonad ()
      selectIRLabeledInstructions [] = return ()

      selectIRLabeledInstructions (a:as) = do
        got <- get
        let labeledInstruction = execState (selectIRLabeledInstruction a) got
        put (labeledInstruction)
        selectIRLabeledInstructions as

      newBl :: String -> SelectorStateMonad ()
      newBl a = do
        got <- get
        let newNodes = [
              Node (counter got) (BasicBlock a) [(Other, Nothing)],
              Node (counter got + 1) (Opcode (OpcodeCondition ARMBl Nothing)) [(Word, Nothing)]]
        let newEdges = [
              Edge (counter got) (counter got + 1) 0]
        let newGraph = appendGraph [Graph newNodes newEdges] (graphs got)
        put ((setGraph newGraph . setCounter (+2)) got)

      newMemory :: ARMOpcode -> Integer -> Integer -> SelectorStateMonad ()
      newMemory a b c = do
        got <- get
        let newNodes = [
              Node (counter got) Register [(Word, Just (IntegerValue b))],
              Node (counter got + 1) Register [(Word, Just (IntegerValue 13))],
              Node (counter got + 2) Constant [(Word, Just (IntegerValue c))],
              Node (counter got + 3) (Opcode (OpcodeCondition a Nothing)) [(Word, Nothing)]]
        let newEdges = [
              Edge (chain got) (counter got + 3) 0,
              Edge (counter got) (counter got + 3) 0,
              Edge (counter got + 1) (counter got + 3) 0,
              Edge (counter got + 2) (counter got + 3) 0]
        let newGraph = appendGraph [Graph newNodes newEdges] (graphs got)
        put ((setGraph newGraph . setCounter (+4) . setChain (counter got + 3)) got)

      newBinary :: ARMOpcode -> SelectorStateMonad ()
      newBinary a = do
        got <- get
        let newNodes = [
              Node (counter got) Register [(Word, Just (IntegerValue 0))],
              Node (counter got + 1) Register [(Word, Just (IntegerValue 1))],
              Node (counter got + 2) (Opcode (OpcodeCondition a Nothing)) [(Word, Nothing)]]
        let newEdges = [
              Edge (counter got) (counter got + 2) 0,
              Edge (counter got) (counter got + 2) 0,
              Edge (counter got + 1) (counter got + 2) 0]
        let newGraph = appendGraph [Graph newNodes newEdges] (graphs got)
        put ((setGraph newGraph . setCounter (+3)) got)

      binaryInstruction :: SelectorStateMonad () -> Maybe IRLabel -> IRType -> IRValue -> IRValue -> SelectorStateMonad ()
      binaryInstruction a (Just (IRLabelNumber b)) c (IRLabelValue (IRLabelNumber d)) (IRLabelValue (IRLabelNumber e)) = do
        got <- get
        let machineValueType = toMachineValueType c
        let bytes = toBytes machineValueType
        let firstLdr = execState (newMemory ARMLdr 0 (toOffset d bytes)) got
        let secondLdr = execState (newMemory ARMLdr 1 (toOffset e bytes)) firstLdr
        let binary = execState a secondLdr
        let str = execState (newMemory ARMStr 0 (toOffset b bytes)) binary
        put str

      selectIRLabeledInstruction :: (Maybe IRLabel, IRInstruction) -> SelectorStateMonad ()
      selectIRLabeledInstruction (a, IRAdd b c d) = binaryInstruction (newBinary ARMAdd) a b c d
      selectIRLabeledInstruction (a, IRSub b c d) = binaryInstruction (newBinary ARMSub) a b c d
      selectIRLabeledInstruction (a, IRMul b c d) = binaryInstruction (newBinary ARMMul) a b c d
      selectIRLabeledInstruction (a, IRAnd b c d) = binaryInstruction (newBinary ARMAnd) a b c d
      selectIRLabeledInstruction (a, IROr b c d) = binaryInstruction (newBinary ARMOrr) a b c d
      selectIRLabeledInstruction (a, IRXor b c d) = binaryInstruction (newBinary ARMEor) a b c d

      selectIRLabeledInstruction (Just (IRLabelNumber a), IRAlloca b) = do
        got <- get
        let machineValueType = toMachineValueType b
        let bytes = toBytes machineValueType
        let newNodes = [
              Node (counter got) Register [(Word, Just (IntegerValue 13))],
              Node (counter got + 1) Constant [(machineValueType, Just (IntegerValue bytes))],
              Node (counter got + 2) (Opcode (OpcodeCondition ARMSub Nothing)) [(Word, Nothing)]]
        let newEdges = [
              Edge (counter got) (counter got + 2) 0,
              Edge (counter got) (counter got + 2) 0,
              Edge (counter got + 1) (counter got + 2) 0]
        let newGraph = appendGraph [Graph newNodes newEdges] (graphs got)
        put ((setGraph newGraph . setCounter (+3) . setOffset (+bytes)) got)

      selectIRLabeledInstruction (a@(Just (IRLabelNumber b)), IRLoad c (IRLabelValue (IRLabelNumber d))) = do
        got <- get
        let machineValueType = toMachineValueType c
        let bytes = toBytes machineValueType
        let alloca = execState (selectIRLabeledInstruction (a, IRAlloca c)) got
        let ldr = execState (newMemory ARMLdr 0 (toOffset d bytes)) alloca
        let str = execState (newMemory ARMStr 0 (toOffset b bytes)) ldr
        put str

      selectIRLabeledInstruction (Nothing, IRStore b c@(IRConstantValue _) (IRLabelNumber d)) = do
        got <- get
        let nodeValue = toNodeValue c
        let machineValueType = toMachineValueType b
        let bytes = toBytes machineValueType
        let newNodes = [
              Node (counter got) Register [(Word, Just (IntegerValue 0))],
              Node (counter got + 1) Constant [(Word, Just nodeValue)],
              Node (counter got + 2) (Opcode (OpcodeCondition ARMMov Nothing)) [(Word, Nothing)]]
        let newEdges = [
              Edge (counter got) (counter got + 2) 0,
              Edge (counter got + 1) (counter got + 2) 0]
        let newGraph = appendGraph [Graph newNodes newEdges] (graphs got)
        let str = execState (newMemory ARMStr 0 (toOffset d bytes)) ((setGraph newGraph . setCounter (+3)) got)
        put str

      selectIRLabeledInstruction (Nothing, IRStore b (IRLabelValue (IRLabelNumber c)) (IRLabelNumber d)) = do
        got <- get
        let machineValueType = toMachineValueType b
        let bytes = toBytes machineValueType
        let ldr = execState (newMemory ARMLdr 0 (toOffset c bytes)) got
        let str = execState (newMemory ARMStr 0 (toOffset d bytes)) ldr
        put str

  selectIRModule :: IRModule -> SelectorStateMonad [Graph]
  selectIRModule (IRModule a) = do
    got <- get
    let newGraph = (execState (selectIRGlobalValues a) got)
    return ((init . graphs) newGraph)

  select :: IRModule -> [Graph]
  select a = evalState (selectIRModule a) (SelectorState ([Graph [] []]) 0 0 0 "")
