module Allocator where
  import Control.Monad.State
  import Data.List
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Scheduler
  import Selector hiding (counter, offset, Register, setOffset)
  import Syntax

  data LiveInterval = LiveInterval {
    liveFrom :: Integer,
    liveTo :: Integer} deriving (Show, Eq, Ord)

  compareLiveTo (_, LiveInterval b (-1)) (_, LiveInterval c _) = compare b c
  compareLiveTo (_, LiveInterval b _) (_, LiveInterval c (-1)) = compare c b
  compareLiveTo (_, LiveInterval _ b) (_, LiveInterval _ c) = compare b c

  compareLiveFrom (_, LiveInterval b _) (_, LiveInterval c _) = compare b c

  setLiveFrom a (LiveInterval _ b) = LiveInterval a b

  setLiveTo a (LiveInterval b _) = LiveInterval b a

  data AnalyzerState = AnalyzerState {
    counter :: Integer,
    table :: Map Operand LiveInterval}

  type AnalyzerStateMonad = State AnalyzerState

  data AllocatorState = AllocatorState {
    available :: [Integer],
    active :: Map Operand LiveInterval,
    registers :: Map Operand Operand,
    offset :: Integer}

  type AllocatorStateMonad = State AllocatorState

  setAvailable a (AllocatorState _ b c d) = AllocatorState a b c d

  setActive a (AllocatorState b _ c d) = AllocatorState b a c d

  setRegisters a (AllocatorState b c _ d) = AllocatorState b c a d

  setOffset a (AllocatorState b c d e) = AllocatorState b c d (a e)

  registerNumber (Register _ a) = a

  readWrote (MCInstruction (OpcodeCondition ARMMov _) [a, b]) = ([b], [a])
  readWrote (MCInstruction (OpcodeCondition ARMMvn _) [a, b]) = ([b], [a])
  readWrote (MCInstruction (OpcodeCondition ARMMovt _) [a, b]) = ([b], [a])
  readWrote (MCInstruction (OpcodeCondition ARMAdd _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMSub _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMCmp _) [a, b]) = ([], [a, b])
  readWrote (MCInstruction (OpcodeCondition ARMMul _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMBl _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMAnd _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMOrr _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMEor _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMB _) [a]) = ([a], [])
  readWrote (MCInstruction (OpcodeCondition ARMBl _) [a]) = ([a], [Register Physical 0])
  readWrote (MCInstruction (OpcodeCondition ARMBx _) [a]) = ([a], [])
  readWrote (MCInstruction (OpcodeCondition ARMLdr _) [a, b, _]) = ([b], [a])
  readWrote (MCInstruction (OpcodeCondition ARMStr _) [a, b, _]) = ([a], [b])

  analyzeMachineCode a = ((filter isRegister ((fst . readWrote) a)), (filter isRegister ((snd . readWrote) a)))
    where
      isRegister (Register _ _) = True
      isRegister _ = False

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
  analyzeOperands a [] = return ()

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
  allocateLiveInterval a@(b@(Register Virtual _), c) = do
    got <- get
    let expired = execState (expireIntervals a ((sortBy compareLiveTo . Map.toList . active) got)) got
    if ((length . available) expired) == 0 then spillInterval a
    else do
      let newAvailable = (tail . available) expired
      let newActive = Map.insert b c (active expired)
      let newRegisters = Map.insert b (Register Physical ((head . available) expired)) (registers expired)
      put ((setAvailable newAvailable . setActive newActive . setRegisters newRegisters) expired)

  allocateLiveInterval a@(b@(Register Physical _), c) = do
    got <- get
    let newRegisters = Map.insert b b (registers got)
    put ((setRegisters newRegisters) got)

  expireIntervals :: (Operand, LiveInterval) -> [(Operand, LiveInterval)] -> AllocatorStateMonad ()
  expireIntervals _ [] = return ()

  expireIntervals a (b:bs) = do
    got <- get
    let expired = execState (expireInterval a b) got
    put expired
    expireIntervals a bs

  expireInterval :: (Operand, LiveInterval) -> (Operand, LiveInterval) -> AllocatorStateMonad ()
  expireInterval (a, b) (c, d) = do
    got <- get
    if (liveTo d) >= (liveFrom b) || ((liveTo d) == (-1) && (liveFrom d) >= (liveFrom b)) then return ()
    else do
      let newAvailable = [registerNumber ((registers got) Map.! c)] ++ (available got)
      let newActive = Map.delete c (active got)
      put ((setAvailable newAvailable . setActive newActive) got)

  spillInterval :: (Operand, LiveInterval) -> AllocatorStateMonad ()
  spillInterval (a, b) = do
    got <- get
    let spill = (last . Map.toList . active) got
    if (liveTo . snd) spill > liveTo b then do
      let newActive = Map.insert a b (Map.delete (fst spill) (active got))
      let firstNewRegisters = Map.insert a ((registers got) Map.! (fst spill)) (registers got)
      let secondNewRegisters = Map.insert (fst spill) (Address (offset got)) firstNewRegisters
      put ((setActive newActive . setRegisters secondNewRegisters . setOffset (+4)) got)
    else do
      let newRegisters = Map.insert a (Address (offset got)) (registers got)
      put ((setRegisters newRegisters . setOffset (+4)) got)

  allocateMachineCodes a = addSubs [] machineCodes
    where
      toPhysical = registers (execState (allocateLiveIntervals ((sortBy compareLiveFrom . analyze) a)) (AllocatorState [2, 3, 9, 10, 11] Map.empty Map.empty 0))
      machineCodes = map instruction a
      operand b@(Register _ _) = toPhysical Map.! b
      operand b = b
      instruction (MCInstruction b c) = MCInstruction b (map operand c)
      instruction b = b
      addSub b = [MCInstruction (OpcodeCondition b Nothing) [Register Physical 13, Register Physical 13, Immediate stackOffset]]
      stackOffset = ((*4) . fromIntegral . length . filter address . Map.toList) toPhysical
      address (_, Address _) = True
      address _ = False
      addSubs b [] = b
      addSubs b (c@(MCSymbol (MCGlobal MCFunction) _):d) = addSubs (b ++ [c] ++ addSub ARMSub) d
      addSubs b (c@(MCInstruction (OpcodeCondition ARMBx Nothing) [Register Physical 14]):d) = addSubs (b ++ addSub ARMAdd ++ [c]) d
      addSubs b (c:d) = addSubs (b ++ [c]) d

  resolveMachineCodes a [] = a
  resolveMachineCodes a (b@(MCInstruction c (d:e)):f) = resolveMachineCodes (a ++ (firstMachineCodes (fst operands) b) ++ [MCInstruction c (snd operands)] ++ lastMachineCodes b) f
    where
      operands = (resolveOperands ([], []) 4 (d:e))

  resolveMachineCodes a (b:c) = resolveMachineCodes (a ++ [b]) c

  resolveOperands a b [] = a
  resolveOperands a b (Address c:d) = resolveOperands (fst a ++ [MCInstruction (OpcodeCondition ARMLdr Nothing) [Register Physical b, Register Physical 13, Immediate c]], (snd a ++ [Register Physical b])) (b + 1) d
  resolveOperands a b (c:d) = resolveOperands (fst a, (snd a ++ [c])) b d

  firstMachineCodes a (MCInstruction _ (_:b))
    | any address b = a
    | otherwise = []
    where
      address (Address _) = True
      address _ = False

  lastMachineCodes (MCInstruction _ (Address a:_)) = [MCInstruction (OpcodeCondition ARMStr Nothing) [Register Physical 0, Register Physical 13, Immediate a]]
  lastMachineCodes _ = []

  allocate = map (resolveMachineCodes [] . allocateMachineCodes)
