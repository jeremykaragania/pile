module Selector where
  import Control.Monad.State
  import Data.Bits
  import Data.Int
  import Data.Word
  import Syntax
  import Unsafe.Coerce

  data OpcodeCondition = OpcodeCondition {
    opcode :: ARMOpcode,
    condition :: (Maybe ARMCondition)} deriving (Show, Eq)

  data RegisterType =
    Virtual |
    Physical deriving (Show, Eq, Ord)

  data NodeType =
    EntryToken |
    Label String |
    VariableGlobal String |
    FunctionGlobal String |
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

  data Node = Node {
    nodeID :: Integer,
    nodeType :: NodeType,
    nodeValues :: [(MachineValueType, Maybe NodeValue)]} deriving (Show, Eq)

  data Edge = Edge {
    fromNode :: Integer,
    toNode :: Integer,
    result :: Integer} deriving (Show, Eq)

  data Graph = Graph {
    nodes :: [Node],
    edges :: [Edge]} deriving (Show, Eq)

  appendGraph a b = init b ++ [append (last b) (head a)] ++ tail a
    where append a b = Graph (nodes a ++ nodes b) (edges a ++ edges b)

  {-
    SelectorState carries state between selectors. A SelectorState carries a directed acyclic graphs (graph) which is a graph
    representation of the intermediate representation, an accumulator (counter) for the numbering of graph nodes, the most
    recent side-effecting node (chain), the stack pointer offset (offset), and the global value name (global).
  -}
  data SelectorState = SelectorState {
    graphs :: [Graph],
    counter :: Integer,
    chain :: Integer,
    offset :: Integer,
    global :: String}

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

  fromIRFCondition IRFOeq = ARMEq
  fromIRFCondition IRFOgt = ARMGt
  fromIRFCondition IRFOge = ARMGe
  fromIRFCondition IRFOlt = ARMLt
  fromIRFCondition IRFOle = ARMLe
  fromIRFCondition IRFOne = ARMNe

  nameFromIRFCondition IRFOeq = "__aeabi_fcmpeq"
  nameFromIRFCondition IRFOgt = "__aeabi_fcmpgt"
  nameFromIRFCondition IRFOge = "__aeabi_fcmpge"
  nameFromIRFCondition IRFOlt = "__aeabi_fcmplt"
  nameFromIRFCondition IRFOle = "__aeabi_fcmple"
  nameFromIRFCondition IRFOne = "__aeabi_fcmpeq"

  fromFloat :: Float -> Word32
  fromFloat = unsafeCoerce

  wordBottom a = a .&. (0x0000ffff :: Word32)
  wordTop a = shift (a .&. (0xffff0000 :: Word32)) (-16)

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
          Node (counter got + 1) (FunctionGlobal b) [(Other, Nothing)]]
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

      newMov :: OpcodeCondition -> Integer -> RegisterType -> NodeType -> [(MachineValueType, Maybe NodeValue)] -> SelectorStateMonad ()
      newMov a b c d e = do
        got <- get
        let newNodes = [
              Node (counter got) (Register c) [(Word, Just (IntegerValue b))],
              Node (counter got + 1) d e,
              Node (counter got + 2) (Opcode a) [(Word, Nothing)]]
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

      newMemory :: ARMOpcode -> Integer -> Integer -> Integer -> SelectorStateMonad ()
      newMemory a b c d = do
        got <- get
        let newNodes = [
              Node (counter got) (Register Virtual) [(Word, Just (IntegerValue b))],
              Node (counter got + 1) (Register Physical) [(Word, Just (IntegerValue c))],
              Node (counter got + 2) Constant [(Word, Just (IntegerValue d))],
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

      newBinaryFunction :: String -> Maybe IRLabel -> IRValue -> IRValue -> SelectorStateMonad ()
      newBinaryFunction a (Just (IRLabelNumber b)) (IRLabelValue (IRLabelNumber c)) (IRLabelValue (IRLabelNumber d)) = do
        got <- get
        let mov0 = execState (newMov (OpcodeCondition ARMMov Nothing) 0 Physical (Register Virtual) [(Word, Just (IntegerValue c))]) got
        let mov1 = execState (newMov (OpcodeCondition ARMMov Nothing) 1 Physical (Register Virtual) [(Word, Just (IntegerValue d))]) mov0
        let bl = execState (newBranch (OpcodeCondition ARMBl Nothing) a) mov1
        let mov2 = execState (newMov (OpcodeCondition ARMMov Nothing) b Virtual (Register Physical) [(Word, Just (IntegerValue 0))]) bl
        put mov2

      newIntegerCompare :: IRValue -> IRValue -> SelectorStateMonad ()
      newIntegerCompare a b = do
        got <- get
        let newNodes = [
              Node (counter got) (toNodeType a) [(Word, Just (toNodeValue a))],
              Node (counter got + 1) (toNodeType b) [(Word, Just (toNodeValue b))],
              Node (counter got + 2) (Opcode (OpcodeCondition ARMCmp Nothing)) [(Word, Nothing)]]
        let newEdges = [
              Edge (counter got) (counter got + 2) 0,
              Edge (counter got + 1) (counter got + 2) 0]
        let newGraph = appendGraph [Graph newNodes newEdges] (graphs got)
        put ((setGraph newGraph . setCounter (+3)) got)

      newCast :: String -> Maybe IRLabel -> IRValue -> SelectorStateMonad ()
      newCast a (Just (IRLabelNumber b)) (IRLabelValue (IRLabelNumber c)) = do
        got <- get
        let mov0 = execState (newMov (OpcodeCondition ARMMov Nothing) 0 Physical (Register Virtual) [(Word, Just (IntegerValue c))]) got
        let bl = execState (newBranch (OpcodeCondition ARMBl Nothing) a) mov0
        let mov1 = execState (newMov (OpcodeCondition ARMMov Nothing) b Virtual (Register Physical) [(Word, Just (IntegerValue 0))]) bl
        put mov1

      selectIRLabeledInstruction :: (Maybe IRLabel, IRInstruction) -> SelectorStateMonad ()
      selectIRLabeledInstruction (a, IRAdd b c d) = newBinary ARMAdd a c d
      selectIRLabeledInstruction (a, IRFadd b c d) = newBinaryFunction "__aeabi_fadd" a c d
      selectIRLabeledInstruction (a, IRSub b c d) = newBinary ARMSub a c d
      selectIRLabeledInstruction (a, IRFsub b c d) = newBinaryFunction "__aeabi_fsub" a c d
      selectIRLabeledInstruction (a, IRMul b c d) = newBinary ARMMul a c d
      selectIRLabeledInstruction (a, IRFmul b c d) = newBinaryFunction "__aeabi_fmul" a c d
      selectIRLabeledInstruction (a, IRUdiv b c d) = newBinaryFunction "__aeabi_uidiv" a c d
      selectIRLabeledInstruction (a, IRSdiv b c d) = newBinaryFunction "__aeabi_idiv" a c d
      selectIRLabeledInstruction (a, IRFdiv b c d) = newBinaryFunction "__aeabi_fdiv" a c d
      selectIRLabeledInstruction (a, IRUrem b c d) = newBinaryFunction "__aeabi_uidivmod" a c d
      selectIRLabeledInstruction (a, IRSrem b c d) = newBinaryFunction "__aeabi_idivmod" a c d
      selectIRLabeledInstruction (a, IRShl b c d) = newBinaryFunction "__aeabi_llsl" a c d
      selectIRLabeledInstruction (a, IRLshr b c d) = newBinaryFunction "__aeabi_lasr" a c d
      selectIRLabeledInstruction (a, IRAshr b c d) = newBinaryFunction "__aeabi_lasr" a c d
      selectIRLabeledInstruction (a, IRAnd b c d) = newBinary ARMAnd a c d
      selectIRLabeledInstruction (a, IROr b c d) = newBinary ARMOrr a c d
      selectIRLabeledInstruction (a, IRXor b c d) = newBinary ARMEor a c d

      selectIRLabeledInstruction (Nothing, IRBrConditional _ _ (IRLabelValue a) (IRLabelValue b)) = do
        got <- get
        let branch0 = execState (newBranch (OpcodeCondition ARMB (Just ARMNe)) (toLabel (global got) a)) got
        let branch1 = execState (newBranch (OpcodeCondition ARMB Nothing) (toLabel (global got) b)) branch0
        put branch1

      selectIRLabeledInstruction (Nothing, IRBrUnconditional (IRLabelValue a)) = do
        got <- get
        let branch = execState (newBranch (OpcodeCondition ARMB Nothing) (toLabel (global got) a)) got
        put branch

      selectIRLabeledInstruction (Nothing, IRSwitch _ a b c) = do
        got <- get
        let cmps = execState (comparisons c) got
        let branch = execState (newBranch (OpcodeCondition ARMB Nothing) (toLabel (global got) b)) cmps
        put branch
        where
          comparisons :: [(IRType, IRConstant, IRLabel)] -> SelectorStateMonad ()
          comparisons [] = return ()

          comparisons ((_, d, e):es) = do
            got <- get
            let cmp = execState (newIntegerCompare (IRLabelValue a) (IRConstantValue d)) got
            let branch = execState (newBranch (OpcodeCondition ARMB (Just ARMEq)) (toLabel (global got) e)) cmp
            put branch
            comparisons es

      selectIRLabeledInstruction (Just (IRLabelNumber a), IRAlloca b) = return ()

      selectIRLabeledInstruction (a@(Just (IRLabelNumber b)), IRLoad c (IRLabelValue (IRLabelName d))) = do
        got <- get
        let mov = execState (newMov (OpcodeCondition ARMMov Nothing) 0 Physical (Label d) [(Other, Nothing)]) got
        let ldr = execState (newMemory ARMLdr b 0 0) mov
        put ldr

      selectIRLabeledInstruction (a@(Just (IRLabelNumber b)), IRLoad c (IRLabelValue (IRLabelNumber d))) = do
        got <- get
        let mov = execState (newMov (OpcodeCondition ARMMov Nothing) b Virtual (Register Virtual) [(Word, Just (IntegerValue d))]) got
        put mov

      selectIRLabeledInstruction (Nothing, IRStore b (IRLabelValue (IRLabelNumber c)) (IRLabelName d)) = do
        got <- get
        let mov = execState (newMov (OpcodeCondition ARMMov Nothing) 0 Physical (Label d) [(Other, Nothing)]) got
        let str = execState (newMemory ARMStr c 0 0) mov
        put str

      selectIRLabeledInstruction (Nothing, IRStore (IRInteger _) c@(IRConstantValue _) (IRLabelNumber d)) = do
        got <- get
        let mov = execState (newMov (OpcodeCondition ARMMov Nothing) d Virtual Constant [(Word, Just (toNodeValue c))]) got
        put mov

      selectIRLabeledInstruction (Nothing, IRStore IRFloat (IRConstantValue (IRFloatingConstant c)) (IRLabelNumber d)) = do
        got <- get
        let word = (fromFloat . realToFrac) c
        let bottom = (fromIntegral . wordBottom) word
        let top = (fromIntegral . wordTop) word
        let mov0 = execState (newMov (OpcodeCondition ARMMov Nothing) d Virtual Constant [(Word, Just (IntegerValue bottom))]) got
        let mov1 = execState (newMov (OpcodeCondition ARMMovt Nothing) d Virtual Constant [(Word, Just (IntegerValue top))]) mov0
        put mov1

      selectIRLabeledInstruction (Nothing, IRStore b (IRLabelValue (IRLabelNumber c)) (IRLabelNumber d)) = do
        got <- get
        let mov = execState (newMov (OpcodeCondition ARMMov Nothing) d Virtual (Register Virtual) [(Word, Just (IntegerValue c))]) got
        put mov

      selectIRLabeledInstruction (Just (IRLabelNumber a), IRIcmp b _ c d) = do
        got <- get
        let cmp = execState (newIntegerCompare c d) got
        let mov0 = execState (newMov (OpcodeCondition ARMMov Nothing) a Virtual (Constant) [(Word, Just (IntegerValue 0))]) cmp
        let mov1 = execState (newMov (OpcodeCondition ARMMov (Just (fromIRICondition b))) a Virtual (Constant) [(Word, Just (IntegerValue 1))]) mov0
        put mov1

      selectIRLabeledInstruction ((Just (IRLabelNumber a)), IRFcmp b _ (IRLabelValue (IRLabelNumber c)) (IRLabelValue (IRLabelNumber d))) = do
        got <- get
        let mov0 = execState (newMov (OpcodeCondition ARMMov Nothing) 0 Physical (Register Virtual) [(Word, Just (IntegerValue c))]) got
        let mov1 = execState (newMov (OpcodeCondition ARMMov Nothing) 1 Physical (Register Virtual) [(Word, Just (IntegerValue d))]) mov0
        let bl = execState (newBranch (OpcodeCondition ARMBl Nothing) (nameFromIRFCondition b)) mov1
        if (b == IRFOne) then do
          let mov2 = execState (newMov (OpcodeCondition ARMMvn Nothing) a Virtual (Register Physical) [(Word, Just (IntegerValue 0))]) bl
          put mov2
        else do
          let mov2 = execState (newMov (OpcodeCondition ARMMov Nothing) a Virtual (Register Physical) [(Word, Just (IntegerValue 0))]) bl
          put mov2

      selectIRLabeledInstruction (a, IRFptoui IRFloat b _) = newCast "__aeabi_f2uiz" a b
      selectIRLabeledInstruction (a, IRFptosi IRFloat b _) = newCast "__aeabi_f2iz" a b
      selectIRLabeledInstruction (a, IRSitofp _ b IRFloat) = newCast "__aeabi_i2f" a b
      selectIRLabeledInstruction (a, IRUitofp _ b IRFloat) = newCast "__aeabi_ui2f" a b

  selectIRGlobalValue (IRVariableGlobal a b c) = do
    got <- get
    let newNodes = [
          Node (counter got) (VariableGlobal a) [(toMachineValueType b, Just (toNodeValue (IRConstantValue c)))]]
    let newGraph = appendGraph [Graph newNodes []] (graphs got)
    put ((setGraph newGraph . setCounter (+1)) got)

  selectIRModule :: IRModule -> SelectorStateMonad [Graph]
  selectIRModule (IRModule a) = do
    got <- get
    let newGraph = (execState (selectIRGlobalValues a) got)
    return ((init . graphs) newGraph)

  select :: IRModule -> [Graph]
  select a = evalState (selectIRModule a) (SelectorState ([Graph [] []]) 0 0 0 "")
