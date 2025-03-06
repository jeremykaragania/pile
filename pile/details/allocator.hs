module Allocator where
  import Control.Monad.State
  import Data.List
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Scheduler
  import Selector hiding (counter, offset, Reg, setOffset)
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
    read :: [Operand],
    wrote :: [Operand]
  }

  {-
    AnalyzerState is a type for the analyzer it keeps track of the current line
    number "counter"; and a lookup table "table" which associates an operand
    with its live interval information.
  -}
  data AnalyzerState = AnalyzerState {
    counter :: Integer,
    table :: Map Operand LiveInterval}

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

  readWrote (MCInstruction (OpcodeCondition ARMMov _) [a, b]) = ([b], [a])
  readWrote (MCInstruction (OpcodeCondition ARMMvn _) [a, b]) = ([b], [a])
  readWrote (MCInstruction (OpcodeCondition ARMMovt _) [a, b]) = ([b], [a])
  readWrote (MCInstruction (OpcodeCondition ARMVmov _) [a, b]) = ([b], [a])
  readWrote (MCInstruction (OpcodeCondition ARMAdd _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMSub _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMCmp _) [a, b]) = ([a, b], [])
  readWrote (MCInstruction (OpcodeCondition ARMMul _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMBl _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMAnd _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMOrr _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMEor _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMB _) [a]) = ([a], [])
  readWrote (MCInstruction (OpcodeCondition ARMBl _) [a]) = ([a], [Reg (integerReg physReg) 0])
  readWrote (MCInstruction (OpcodeCondition ARMBx _) [a]) = ([a], [])
  readWrote (MCInstruction (OpcodeCondition ARMLdr _) [a, b, _]) = ([b], [a])
  readWrote (MCInstruction (OpcodeCondition ARMStr _) [a, b, _]) = ([a], [b])
  readWrote (MCInstruction (OpcodeCondition ARMPush _) a) = (a, [])
  readWrote (MCInstruction (OpcodeCondition ARMPop _) a) = ([], a)

  {-
    machineCodeToBlocks groups instructions by their block to be analyzed
    later.
  -}
  machineCodeToBlocks :: Map String [MachineCode] -> String -> [MachineCode] -> Map String [MachineCode]
  machineCodeToBlocks m s (MCSymbol _ a:as) = machineCodeToBlocks (Map.insert a [] m) a as
  machineCodeToBlocks m s (a@(MCInstruction _ _):as) = machineCodeToBlocks (Map.insert s ((m Map.! s) ++ [a]) m) s as
  machineCodeToBlocks m s (_:as) = machineCodeToBlocks m s as
  machineCodeToBlocks m _ [] = m

  analyzeMachineCode a = ((filter isReg ((fst . readWrote) a)), (filter isReg ((snd . readWrote) a)))
    where
      isReg (Reg _ _) = True
      isReg _ = False

  analyzeMachineCodes a = map analyzeMachineCode (filter isInstruction a)
    where
      isInstruction (MCInstruction _ _) = True
      isInstruction _ = False

  analyzeOperandsPairs :: [([Operand], [Operand])] -> AnalyzerStateMonad ()
  analyzeOperandsPairs [] = return ()

  analyzeOperandsPairs (a:as) = do
    got <- get
    let analyzed = execState (analyzeOperandsPair a) got
    put analyzed
    analyzeOperandsPairs as

  analyzeOperandsPair :: ([Operand], [Operand]) -> AnalyzerStateMonad ()
  analyzeOperandsPair (a, b) = do
    got <- get
    let analyzedRead = execState (analyzeOperands setLiveTo a) got
    let analyzedWrote = execState (analyzeOperands setLiveFrom b) analyzedRead
    put (AnalyzerState (counter analyzedWrote + 1) (table analyzedWrote))

  analyzeOperands :: (Integer -> LiveInterval -> LiveInterval) -> [Operand] -> AnalyzerStateMonad ()
  analyzeOperands _ [] = return ()

  analyzeOperands b (a:as) = do
    got <- get
    let analyzed = execState (analyzeOperand b a) got
    put analyzed
    analyzeOperands b as

  analyzeOperand :: (Integer -> LiveInterval -> LiveInterval) -> Operand -> AnalyzerStateMonad ()
  analyzeOperand a b = do
    got <- get
    if Map.notMember b (table got) then do
      let newTable = Map.insert b (a (counter got) (LiveInterval (-1) (-1))) (table got)
      put (AnalyzerState (counter got) newTable)
    else do
      let oldValue = (table got) Map.! b
      let newValue = (a (counter got) oldValue)
      let newTable = Map.insert b (whichValue oldValue newValue) (table got)
      put (AnalyzerState (counter got) newTable)
    where
      whichValue c d
        | liveFrom c < liveFrom d = c
        | otherwise = d

  analyze a = (Map.toList . table) (execState (analyzeOperandsPairs (analyzeMachineCodes a)) (AnalyzerState 0 Map.empty))

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
      let firstNewRegs = Map.insert a ((regs got) Map.! (fst spill)) (regs got)
      let secondNewRegs = Map.insert (fst spill) (Address (offset got)) firstNewRegs
      put ((setActive newActive . setRegs secondNewRegs . setOffset (+4)) got)
    else do
      let newRegs = Map.insert a (Address (offset got)) (regs got)
      put ((setRegs newRegs . setOffset (+4)) got)

  allocateMachineCodes a = addSubs [] machineCodes
    where
      toPhysicalReg = regs (execState (allocateLiveIntervals ((sortBy compareLiveFrom . analyze) a)) (AllocatorState [2, 3, 9, 10, 11] Map.empty Map.empty 0))
      machineCodes = map instruction a
      operand b@(Reg _ _) = toPhysicalReg Map.! b
      operand b = b
      instruction (MCInstruction b c) = MCInstruction b (map operand c)
      instruction b = b
      addSub b = [MCInstruction (OpcodeCondition b Nothing) [Reg (integerReg physReg) 13, Reg (integerReg physReg) 13, Immediate stackOffset]]
      stackOffset = ((*4) . fromIntegral . length . filter address . Map.toList) toPhysicalReg
      address (_, Address _) = True
      address _ = False
      addSubs b [] = b
      addSubs b (c@(MCSymbol (MCGlobal MCFunction) _):d) = addSubs (b ++ [c] ++ addSub ARMSub) d
      addSubs b (c@(MCInstruction (OpcodeCondition ARMBx Nothing) [Reg (RegType IntegerReg PhysicalReg) 14]):d) = addSubs (b ++ addSub ARMAdd ++ [c]) d
      addSubs b (c:d) = addSubs (b ++ [c]) d

  resolveMachineCodes a [] = a
  resolveMachineCodes a (b@(MCInstruction c (d:e)):f) = resolveMachineCodes (a ++ (firstMachineCodes (fst operands) b) ++ [MCInstruction c (snd operands)] ++ lastMachineCodes b) f
    where
      operands = (resolveOperands ([], []) 4 (d:e))

  resolveMachineCodes a (b:c) = resolveMachineCodes (a ++ [b]) c

  resolveOperands a _ [] = a
  resolveOperands a b (Address c:d) = resolveOperands (fst a ++ [MCInstruction (OpcodeCondition ARMLdr Nothing) [Reg (integerReg physReg) b, Reg (integerReg physReg) 13, Immediate c]], (snd a ++ [Reg (integerReg physReg) b])) (b + 1) d
  resolveOperands a b (c:d) = resolveOperands (fst a, (snd a ++ [c])) b d

  firstMachineCodes a (MCInstruction _ (_:b))
    | any address b = a
    | otherwise = []
    where
      address (Address _) = True
      address _ = False

  lastMachineCodes (MCInstruction _ [Address a, b]) = [MCInstruction (OpcodeCondition ARMStr Nothing) [Reg (integerReg physReg) (opcodeNumber b), Reg (integerReg physReg) 13, Immediate a]]
  lastMachineCodes _ = []

  allocate = map (resolveMachineCodes [] . allocateMachineCodes)
