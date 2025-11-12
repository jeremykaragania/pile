module Allocator where
  import Control.Monad.State
  import Data.List
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Data.Set (Set)
  import qualified Data.Set as Set
  import Scheduler
  import Selector hiding (counter, offset, Reg, setOffset, Label)
  import Syntax

  {-
    LiveInterval is a type for the liveness of a variable. A variable is live
    if it stores a value that may be needed in the future. Here, "liveFrom" is
    the point when the variable begins being live; and "liveEnd" is the point
    where the variable ends being live.
  -}
  data LiveInterval = LiveInterval {
    liveFrom :: Integer,
    liveTo :: Integer} deriving (Show, Eq, Ord)

  compareLiveTo (_, LiveInterval b (-1)) (_, LiveInterval c _) = compare b c
  compareLiveTo (_, LiveInterval b _) (_, LiveInterval c (-1)) = compare c b
  compareLiveTo (_, LiveInterval _ b) (_, LiveInterval _ c) = compare b c

  compareLiveFrom (_, LiveInterval b _) (_, LiveInterval c _) = compare b c

  setLiveFrom a (LiveInterval _ b) = LiveInterval a b

  setLiveTo a (LiveInterval b _) = LiveInterval b a

  {-
    OperandAccessInfo represents which operands are read from and written to in
    an instruction.
  -}
  data OperandAccessInfo = OperandAccessInfo {
    code :: MachineCode,
    readOps :: [Operand],
    wroteOps :: [Operand]
  } deriving (Show)

  {-
    AnalyzerState is a type for the analyzer it keeps track of the current line
    number "counter"; a lookup table "blockTable" which associates an block
    label with the set of operands in that block tracked by the analyzer; and
    another lookup table "intervalTable" which associates an operand with its
    live interval information.
  -}
  data AnalyzerState = AnalyzerState {
    counter :: Integer,
    blockTable :: Map (String, Integer) (Set Operand),
    intervalTable :: Map Operand LiveInterval}

  type AnalyzerStateMonad = State AnalyzerState

  {-
    AllocatorState is a type for the allocator which is a linear scan register
    allocator. It keeps track of the registers available be allocated
    "available"; a lookup table "active" which associates an operand with its
    live interval information; a lookup table "regs" which associates an
    unallocated operand and an allocated operand; and the current stack offset
    "offset".
  -}
  data AllocatorState = AllocatorState {
    available :: [Integer],
    active :: Map Operand LiveInterval,
    regs :: Map Operand Operand,
    offset :: Integer}

  type AllocatorStateMonad = State AllocatorState

  setAvailable a (AllocatorState _ b c d) = AllocatorState a b c d

  setActive a (AllocatorState b _ c d) = AllocatorState b a c d

  setRegs a (AllocatorState b c _ d) = AllocatorState b c a d

  setOffset a (AllocatorState b c d e) = AllocatorState b c d (a e)

  opcodeNumber (Reg _ a) = a
  opcodeNumber (Address a) = a

  {-
    readWrote determines which operands are read and written to in an
    instruction for use in live interval calculation.
  -}
  readWrote (OpcodeCondition ARMMov _) [a, b] = ([b], [a])
  readWrote (OpcodeCondition ARMMvn _) [a, b] = ([b], [a])
  readWrote (OpcodeCondition ARMMovt _) [a, b] = ([b], [a])
  readWrote (OpcodeCondition ARMVmov _) [a, b] = ([b], [a])
  readWrote (OpcodeCondition ARMAdd _) [a, b, c] = ([b, c], [a])
  readWrote (OpcodeCondition ARMSub _) [a, b, c] = ([b, c], [a])
  readWrote (OpcodeCondition ARMCmp _) [a, b] = ([a, b], [])
  readWrote (OpcodeCondition ARMMul _) [a, b, c] = ([b, c], [a])
  readWrote (OpcodeCondition ARMBl _) [a, b, c] = ([b, c], [a])
  readWrote (OpcodeCondition ARMAnd _) [a, b, c] = ([b, c], [a])
  readWrote (OpcodeCondition ARMOrr _) [a, b, c] = ([b, c], [a])
  readWrote (OpcodeCondition ARMEor _) [a, b, c] = ([b, c], [a])
  readWrote (OpcodeCondition ARMB _) [a] = ([a], [])
  readWrote (OpcodeCondition ARMBl _) [a] = ([a], [Reg (integerReg physReg) 0])
  readWrote (OpcodeCondition ARMBx _) [a] = ([a], [])
  readWrote (OpcodeCondition ARMLdr _) [a, b, _] = ([b], [a])
  readWrote (OpcodeCondition ARMStr _) [a, b, _] = ([a], [b])
  readWrote (OpcodeCondition ARMPush _) a = (a, [])
  readWrote (OpcodeCondition ARMPop _) a = ([], a)

  filterBasicBlocks = filter isBasicBlock

  isBasicBlock (MCBasicBlock _ _) = True
  isBasicBlock _ = False

  basicBlockNames mcs = map (\(MCBasicBlock x _) -> x) (filterBasicBlocks mcs)

  {-
    createBasicBlockMap turn basic blocks into a map keyed by the basic block
    name for later use.
  -}
  createBasicBlockMap mcs = go
    where
      go = Map.fromList (map toPair (filterBasicBlocks mcs))

      toPair (MCBasicBlock name instrs) = (name, instrs)

  analyzeMachineCode a@(MCInstruction b c) = OperandAccessInfo a (filter isReg (fst (readWrote b c))) (filter isReg (snd (readWrote b c)))
    where
      isReg (Reg _ _) = True
      isReg _ = False

  {-
    analyzeMachineCodes analyze which operands each instruction writes to and
    reads from.
  -}
  analyzeMachineCodes mcs = go
    where
      go = map (map analyzeMachineCode) instrs
      basicBlocks = filterBasicBlocks mcs
      instrs = map (\(MCBasicBlock _ x) -> x) basicBlocks

  {-
    analyzeOpsPairs calculates the live intervals for each operand.
  -}
  analyzeOpsPairs :: [((String, Integer), OperandAccessInfo)] -> AnalyzerStateMonad ()
  analyzeOpsPairs [] = return ()

  analyzeOpsPairs (a:as) = do
    got <- get
    let analyzed = execState (analyzeOpsPair a) got
    put analyzed
    analyzeOpsPairs as

  analyzeOpsPair :: ((String, Integer), OperandAccessInfo) -> AnalyzerStateMonad ()
  analyzeOpsPair (label@(name, number), o@(OperandAccessInfo instr@(MCInstruction (OpcodeCondition opcode _) operands) r w))
    -- Every operand's end interval in the branch block has to be extended to
    -- the current point.
    | isBackwardsBranch = do
      got <- get
      let extended = execState (extendIntervalsTo (branchName, branchNumber) (counter got)) got
      put (AnalyzerState (counter extended + 1) (blockTable extended) (intervalTable extended))
    | otherwise = do
        got <- get
        let analyzedRead = execState (analyzeOps setLiveTo label instr r) got
        let analyzedWrote = execState (analyzeOps setLiveFrom label instr w) analyzedRead
        put (AnalyzerState (counter analyzedWrote + 1) (blockTable analyzedWrote) (intervalTable analyzedWrote))
    where
      isBackwardsBranch = isBranch opcode && name == branchName && number > branchNumber

      branchName = labelName operands
      branchNumber = labelNumber operands

      labelName [Label x _] = x
      labelNumber [Label _ (Just x)] = x

      isBranch ARMB = True
      isBranch _ = False

  analyzeOps :: (Integer -> LiveInterval -> LiveInterval) -> (String, Integer) -> MachineCode -> [Operand] -> AnalyzerStateMonad ()
  analyzeOps _ _ _ [] = return ()

  analyzeOps f label instr (o:os) = do
    got <- get
    let analyzed = execState (analyzeOp f label o) got
    put analyzed
    analyzeOps f label instr os
    return ()

  analyzeOp :: (Integer -> LiveInterval -> LiveInterval) -> (String, Integer) -> Operand -> AnalyzerStateMonad ()
  analyzeOp f label op = do
    got <- get
    let iTbl = intervalTable got
    let bTbl = blockTable got

    let set = bTbl Map.! label
    let newSet = Set.insert op set
    let newBlockTable = Map.insert label newSet bTbl

    if Map.notMember op (intervalTable got) then do
      let newTable = Map.insert op (f (counter got) (LiveInterval (-1) (-1))) iTbl
      put (AnalyzerState (counter got) newBlockTable newTable)
    else do
      let oldValue = iTbl Map.! op
      let newValue = (f (counter got) oldValue)
      let newTable = Map.insert op (whichValue oldValue newValue) iTbl
      put (AnalyzerState (counter got) newBlockTable newTable)
    where
      whichValue c d
        | liveFrom c < liveFrom d = c
        | otherwise = d

  analyze mcs = Map.toList $ intervalTable $ (execState (analyzeOpsPairs opsPairsList)) (AnalyzerState 0 initTable Map.empty)
    where
      initTable = Map.fromList $ zip names (repeat Set.empty)

      opsPairsList = concat $ map (\(name, ops) -> zip (repeat name) ops) (zip names opsPairs)
      opsPairs = analyzeMachineCodes mcs
      names = basicBlockNames mcs

  {-
    extendIntervalsTo extends the live intervals of operands inside a basic
    block with the label "label" to end at "to". Sometimes we need to extend a
    "previous's" block's live intervals as we may be branching back to it.
  -}
  extendIntervalsTo :: (String, Integer) -> Integer -> AnalyzerStateMonad()
  extendIntervalsTo label to = do
    got <- get
    let bTbl = blockTable got
    let iTbl = intervalTable got
    let operands = Set.toList $ bTbl Map.! label
    let extended = Map.fromList $ extendOperands iTbl operands
    let newTable = Map.union extended iTbl

    put (AnalyzerState (counter got) (blockTable got) newTable)
    where
      extendOperands table operands = map (extendOperand table) operands
      extendOperand table operand =
        let interval = table Map.! operand
        in (operand, LiveInterval (liveFrom interval) to)

  allocateLiveIntervals :: [(Operand, LiveInterval)] -> AllocatorStateMonad ()
  allocateLiveIntervals [] = return ()

  allocateLiveIntervals (a:as) = do
    got <- get
    let allocated = execState (allocateLiveInterval a) got
    put allocated
    allocateLiveIntervals as

  allocateLiveInterval :: (Operand, LiveInterval) -> AllocatorStateMonad ()
  allocateLiveInterval a@(b@(Reg (RegType IntegerReg VirtualReg) _), c) = do
    got <- get
    let expired = execState (expireIntervals a ((sortBy compareLiveTo . Map.toList . active) got)) got
    if ((length . available) expired) == 0 then spillInterval a
    else do
      let newAvailable = (tail . available) expired
      let newActive = Map.insert b c (active expired)
      let newRegs = Map.insert b (Reg (integerReg physReg) ((head . available) expired)) (regs expired)
      put ((setAvailable newAvailable . setActive newActive . setRegs newRegs) expired)

  allocateLiveInterval (b@(Reg (RegType _ PhysicalReg) _), _) = do
    got <- get
    let newRegs = Map.insert b b (regs got)
    put ((setRegs newRegs) got)

  expireIntervals :: (Operand, LiveInterval) -> [(Operand, LiveInterval)] -> AllocatorStateMonad ()
  expireIntervals _ [] = return ()

  expireIntervals a (b:bs) = do
    got <- get
    let expired = execState (expireInterval a b) got
    put expired
    expireIntervals a bs

  expireInterval :: (Operand, LiveInterval) -> (Operand, LiveInterval) -> AllocatorStateMonad ()
  expireInterval (_, b) (c, d) = do
    got <- get
    if (liveTo d) >= (liveFrom b) || ((liveTo d) == (-1) && (liveFrom d) >= (liveFrom b)) then return ()
    else do
      let newAvailable = [opcodeNumber ((regs got) Map.! c)] ++ (available got)
      let newActive = Map.delete c (active got)
      put ((setAvailable newAvailable . setActive newActive) got)

  spillInterval :: (Operand, LiveInterval) -> AllocatorStateMonad ()
  spillInterval (a, b) = do
    got <- get
    let spill = (last . Map.toList . active) got
    if (liveTo . snd) spill > liveTo b then do
      let newActive = Map.insert a b (Map.delete (fst spill) (active got))
      let firstRegs = Map.insert a ((regs got) Map.! (fst spill)) (regs got)
      let secondRegs = Map.insert (fst spill) (Address (offset got)) firstRegs
      put ((setActive newActive . setRegs secondRegs . setOffset (+4)) got)
    else do
      let newRegs = Map.insert a (Address (offset got)) (regs got)
      put ((setRegs newRegs . setOffset (+4)) got)

  allocateMachineCodes a = addSubs [] machineCodes
    where
      toPhysicalReg = regs (execState (allocateLiveIntervals ((sortBy compareLiveFrom . analyze) a)) (AllocatorState [2, 3, 9, 10, 11] Map.empty Map.empty reserveSize))

      machineCodes = concat $ map instruction a

      operand b@(Reg _ _) = toPhysicalReg Map.! b
      operand b = b

      instruction (MCBasicBlock label instrs) = [MCSymbol MCLocal (uncurry showLabel label)] ++ map (\(MCInstruction b c) -> MCInstruction b (map operand c)) instrs
      instruction b = [b]

      addSub b = [MCInstruction (OpcodeCondition b Nothing) [Reg (integerReg physReg) 13, Reg (integerReg physReg) 13, Immediate stackOffset]]

      -- 12 bytes: Enough space for three registers.
      reserveSize = 12

      stackOffset = ((+reserveSize) . (*4) . fromIntegral . length . filter address . Map.toList) toPhysicalReg

      address (_, Address _) = True
      address _ = False

      addSubs b [] = b
      addSubs b (c@(MCSymbol (MCGlobal MCFunction) _):d) = addSubs (b ++ [c] ++ addSub ARMSub) d
      addSubs b (c@(MCInstruction (OpcodeCondition ARMBx Nothing) [Reg (RegType IntegerReg PhysicalReg) 14]):d) = addSubs (b ++ addSub ARMAdd ++ [c]) d
      addSubs b (c:d) = addSubs (b ++ [c]) d

  {-
   resolveMachineCodes does a final pass over the machine code and resolves
   register spilling artifacts. During register allocation when we had to
   spill, we didn't worry about the semantics of what that looked like. We
   simply replaced registers with addresses. Since memory can't be an operand,
   we have to do some loads and stores to get those addresses into registers
   first.
  -}
  resolveMachineCodes list [] = list
  resolveMachineCodes list (c:cs) = resolveMachineCodes (list ++ (resolveMachineCode c)) cs

  resolveMachineCode code@(MCInstruction opcode operands) = go
    where
      go = pushes ++ loads ++ [newCode] ++ stores ++ pops

      -- We call them "pushes" and "pops" but we don't actually use push or pop
      -- instructions. It's really just register preservation and restoration.
      -- Since at this point we have no physical registers available, we've
      -- saved some space on the stack for up to three registers. This space is
      -- used to preserve register r0-r2 so they can be used to resolve our
      -- address operations.
      pushes = pushInstrs 0 [] operands
      pops = popInstrs 0 [] operands

      loads = loadInstrs 0 [] operands
      stores = storeInstrs 0 [] operands
      newCode = MCInstruction opcode newOperands
      newOperands = replaceOperands 0 [] operands

      pushInstrs _ instrs [] = instrs
      pushInstrs num instrs (Address _:os) = pushInstrs (num + 1) (instrs ++ [MCInstruction (OpcodeCondition ARMStr Nothing) [Reg (integerReg physReg) num, Reg (integerReg physReg) 13, Immediate (4 * num)]]) os
      pushInstrs num instrs (o:os) = pushInstrs num instrs os

      popInstrs _ instrs [] = instrs
      popInstrs num instrs (Address _:os) = popInstrs (num + 1) (instrs ++ [MCInstruction (OpcodeCondition ARMLdr Nothing) [Reg (integerReg physReg) num, Reg (integerReg physReg) 13, Immediate (4 * num)]]) os
      popInstrs num instrs (o:os) = popInstrs num instrs os

      loadInstrs _ instrs [] = instrs
      loadInstrs num instrs (Address offset:os) = loadInstrs (num + 1) (instrs ++ [MCInstruction (OpcodeCondition ARMLdr Nothing) [Reg (integerReg physReg) num, Reg (integerReg physReg) 13, Immediate offset]]) os
      loadInstrs num instrs (o:os) = loadInstrs num instrs os

      storeInstrs _ instrs [] = instrs
      storeInstrs num instrs (Address offset:os) = storeInstrs (num + 1) (instrs ++ [MCInstruction (OpcodeCondition ARMStr Nothing) [Reg (integerReg physReg) num, Reg (integerReg physReg) 13, Immediate offset]]) os
      storeInstrs num instrs (o:os) = storeInstrs num instrs os

      replaceOperands _ new [] = new
      replaceOperands num new (Address _:os) = replaceOperands (num + 1) (new ++ [Reg (RegType IntegerReg PhysicalReg) num]) os
      replaceOperands num new (o:os) = replaceOperands num (new ++ [o]) os

  resolveMachineCode code = [code]

  allocate = resolveMachineCodes [] . allocateMachineCodes
