module Selector where
  import Control.Monad.State
  import Data.Int
  import Syntax

  data OpcodeCondition = OpcodeCondition {opcode :: ARMOpcode, condition :: (Maybe ARMCondition)} deriving (Show, Eq)

  data RegisterType =
    Virtual |
    Physical deriving (Show, Eq, Ord)

  data NodeType =
    EntryToken |
    Label String |
    BasicBlock String |
    Register RegisterType |
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

  toNodeType (IRConstantValue _) = Constant
  toNodeType (IRLabelValue _) = Register Virtual

  toMachineValueType (IRShortInteger _) = Halfword
  toMachineValueType (IRInteger _) = Word
  toMachineValueType (IRLongInteger _) = Word

  toNodeValue (IRConstantValue (IRIntegerConstant a)) = IntegerValue a
  toNodeValue (IRConstantValue (IRFloatingConstant a)) = FloatingValue a
  toNodeValue (IRLabelValue (IRLabelNumber a)) = IntegerValue a

  toBytes Byte = 1
  toBytes Halfword = 2
  toBytes Word = 4

  toOffset a b = (a - 1) * b

  toLabelString (IRLabelName a) = a
  toLabelString (IRLabelNumber a) = show a

  toLabel a b = a ++ "L" ++ toLabelString b

  fromIRICondition IRIEq = ARMEq
  fromIRICondition IRINe = ARMNe
  fromIRICondition IRIUgt = ARMGt
  fromIRICondition IRIUge = ARMGe
  fromIRICondition IRIUlt = ARMLt
  fromIRICondition IRIUle = ARMLe
  fromIRICondition IRISgt = ARMGt
  fromIRICondition IRISge = ARMGe
  fromIRICondition IRISlt = ARMLt
  fromIRICondition IRISle = ARMLe

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
        let basicBlock = Node (counter got) (BasicBlock (toLabel (global got) a)) [(Other, Nothing)]
        let newGraph = appendGraph [Graph [basicBlock] []] (graphs got)
        let labeledInstructions = execState (selectIRLabeledInstructions b) ((setGraph newGraph . setCounter (+1)) got)
        put labeledInstructions

      selectIRLabeledInstructions :: [(Maybe IRLabel, IRInstruction)] -> SelectorStateMonad ()
      selectIRLabeledInstructions [] = return ()

      selectIRLabeledInstructions (a:as) = do
        got <- get
        let labeledInstruction = execState (selectIRLabeledInstruction a) got
        put (labeledInstruction)
        selectIRLabeledInstructions as

      newMov :: Integer -> Maybe ARMCondition -> NodeType -> [(MachineValueType, Maybe NodeValue)] -> SelectorStateMonad ()
      newMov a b c d = do
        got <- get
        let newNodes = [
              Node (counter got) (Register Virtual) [(Word, Just (IntegerValue a))],
              Node (counter got + 1) c d,
              Node (counter got + 2) (Opcode (OpcodeCondition ARMMov b)) [(Word, Nothing)]]
        let newEdges = [
              Edge (counter got) (counter got + 2) 0,
              Edge (counter got + 1) (counter got + 2) 0]
        let newGraph = appendGraph [Graph newNodes newEdges] (graphs got)
        put ((setGraph newGraph . setCounter (+3)) got)

      newBranch :: OpcodeCondition -> String -> SelectorStateMonad ()
      newBranch a b = do
        got <- get
        let newNodes = [
              Node (counter got) (Label b) [(Other, Nothing)],
              Node (counter got + 1) (Opcode a) [(Word, Nothing)]]
        let newEdges = [
              Edge (counter got) (counter got + 1) 0]
        let newGraph = appendGraph [Graph newNodes newEdges] (graphs got)
        put ((setGraph newGraph . setCounter (+2)) got)

      newMemory :: ARMOpcode -> Integer -> Integer -> SelectorStateMonad ()
      newMemory a b c = do
        got <- get
        let newNodes = [
              Node (counter got) (Register Virtual) [(Word, Just (IntegerValue b))],
              Node (counter got + 1) (Register Physical) [(Word, Just (IntegerValue 13))],
              Node (counter got + 2) Constant [(Word, Just (IntegerValue c))],
              Node (counter got + 3) (Opcode (OpcodeCondition a Nothing)) [(Word, Nothing)]]
        let newEdges = [
              Edge (chain got) (counter got + 3) 0,
              Edge (counter got) (counter got + 3) 0,
              Edge (counter got + 1) (counter got + 3) 0,
              Edge (counter got + 2) (counter got + 3) 0]
        let newGraph = appendGraph [Graph newNodes newEdges] (graphs got)
        put ((setGraph newGraph . setCounter (+4) . setChain (counter got + 3)) got)

      newBinary :: ARMOpcode -> Maybe IRLabel -> IRValue -> IRValue -> SelectorStateMonad ()
      newBinary a (Just (IRLabelNumber b)) (IRLabelValue (IRLabelNumber c)) (IRLabelValue (IRLabelNumber d)) = do
        got <- get
        let newNodes = [
              Node (counter got) (Register Virtual) [(Word, Just (IntegerValue b))],
              Node (counter got + 1) (Register Virtual) [(Word, Just (IntegerValue c))],
              Node (counter got + 2) (Register Virtual) [(Word, Just (IntegerValue d))],
              Node (counter got + 3) (Opcode (OpcodeCondition a Nothing)) [(Word, Nothing)]]
        let newEdges = [
              Edge (counter got) (counter got + 3) 0,
              Edge (counter got + 1) (counter got + 3) 0,
              Edge (counter got + 2) (counter got + 3) 0]
        let newGraph = appendGraph [Graph newNodes newEdges] (graphs got)
        put ((setGraph newGraph . setCounter (+4)) got)

      newBinaryFunction :: String -> Maybe IRLabel -> SelectorStateMonad ()
      newBinaryFunction a (Just (IRLabelNumber b)) = do
        got <- get
        let bl = execState (newBranch (OpcodeCondition ARMBl Nothing) a) got
        let mov = execState (newMov b Nothing (Register Physical) [(Word, Just (IntegerValue 0))]) bl
        put mov

      selectIRLabeledInstruction :: (Maybe IRLabel, IRInstruction) -> SelectorStateMonad ()
      selectIRLabeledInstruction (a, IRAdd b c d) = newBinary ARMAdd a c d
      selectIRLabeledInstruction (a, IRSub b c d) = newBinary ARMSub a c d
      selectIRLabeledInstruction (a, IRMul b c d) = newBinary ARMMul a c d
      selectIRLabeledInstruction (a, IRUdiv b c d) = newBinaryFunction "__aeabi_uidiv" a
      selectIRLabeledInstruction (a, IRSdiv b c d) = newBinaryFunction "__aeabi_idiv" a
      selectIRLabeledInstruction (a, IRUrem b c d) = newBinaryFunction "__aeabi_uidivmod" a
      selectIRLabeledInstruction (a, IRSrem b c d) = newBinaryFunction "__aeabi_idivmod" a
      selectIRLabeledInstruction (a, IRAnd b c d) = newBinary ARMAnd a c d
      selectIRLabeledInstruction (a, IROr b c d) = newBinary ARMOrr a c d
      selectIRLabeledInstruction (a, IRXor b c d) = newBinary ARMEor a c d

      selectIRLabeledInstruction (Nothing, IRBrConditional _ _ (IRLabelValue a) (IRLabelValue b)) = do
        got <- get
        let firstBranch = execState (newBranch (OpcodeCondition ARMB (Just ARMNe)) (toLabel (global got) a)) got
        let secondBranch = execState (newBranch (OpcodeCondition ARMB Nothing) (toLabel (global got) b)) firstBranch
        put secondBranch

      selectIRLabeledInstruction (Nothing, IRBrUnconditional (IRLabelValue a)) = do
        got <- get
        let branch = execState (newBranch (OpcodeCondition ARMB Nothing) (toLabel (global got) a)) got
        put branch

      selectIRLabeledInstruction (Just (IRLabelNumber a), IRAlloca b) = return ()

      selectIRLabeledInstruction (a@(Just (IRLabelNumber b)), IRLoad c (IRLabelValue (IRLabelNumber d))) = do
        got <- get
        let machineValueType = toMachineValueType c
        let bytes = toBytes machineValueType
        let mov = execState (newMov b Nothing (Register Virtual) [(Word, Just (IntegerValue d))]) got
        put mov

      selectIRLabeledInstruction (Nothing, IRStore b c@(IRConstantValue _) (IRLabelNumber d)) = do
        got <- get
        let nodeValue = toNodeValue c
        let machineValueType = toMachineValueType b
        let bytes = toBytes machineValueType
        let mov = execState (newMov d Nothing Constant [(Word, Just nodeValue)]) got
        put mov

      selectIRLabeledInstruction (Nothing, IRStore b (IRLabelValue (IRLabelNumber c)) (IRLabelNumber d)) = do
        got <- get
        let machineValueType = toMachineValueType b
        let bytes = toBytes machineValueType
        let mov = execState (newMov d Nothing (Register Virtual) [(Word, Just (IntegerValue c))]) got
        put mov

      selectIRLabeledInstruction (Just (IRLabelNumber a), IRIcmp b _ d e) = do
        got <- get
        let newNodes = [
              Node (counter got) (toNodeType d) [(Word, Just (toNodeValue d))],
              Node (counter got + 1) (toNodeType e) [(Word, Just (toNodeValue e))],
              Node (counter got + 2) (Opcode (OpcodeCondition ARMCmp Nothing)) [(Word, Nothing)]]
        let newEdges = [
              Edge (counter got) (counter got + 2) 0,
              Edge (counter got + 1) (counter got + 2) 0]
        let newGraph = appendGraph [Graph newNodes newEdges] (graphs got)
        let firstMov = execState (newMov a Nothing (Constant) [(Word, Just (IntegerValue 0))]) ((setGraph newGraph . setCounter (+3)) got)
        let secondMov = execState (newMov a (Just (fromIRICondition b)) (Constant) [(Word, Just (IntegerValue 1))]) firstMov
        put secondMov

  selectIRModule :: IRModule -> SelectorStateMonad [Graph]
  selectIRModule (IRModule a) = do
    got <- get
    let newGraph = (execState (selectIRGlobalValues a) got)
    return ((init . graphs) newGraph)

  select :: IRModule -> [Graph]
  select a = evalState (selectIRModule a) (SelectorState ([Graph [] []]) 0 0 0 "")
