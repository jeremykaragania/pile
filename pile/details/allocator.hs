module Allocator where
  import Control.Monad.State
  import Data.List
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Scheduler
  import Selector
  import Syntax

  data LiveInterval = LiveInterval {liveFrom :: Integer, liveTo :: Integer} deriving (Show, Eq, Ord)

  compareLiveTo (_, LiveInterval b (-1)) (_, LiveInterval c _) = compare b c
  compareLiveTo (_, LiveInterval b _) (_, LiveInterval c (-1)) = compare c b
  compareLiveTo (_, LiveInterval _ b) (_, LiveInterval _ c) = compare b c

  compareLiveFrom (_, LiveInterval b _) (_, LiveInterval c _) = compare b c

  setLiveFrom a (LiveInterval _ b) = LiveInterval a b

  setLiveTo a (LiveInterval b _) = LiveInterval b a

  data AnalyzerState = AnalyzerState {anc :: Integer, table :: Map Operand LiveInterval}

  type AnalyzerStateMonad = State AnalyzerState

  data AllocatorState = AllocatorState {available :: [Integer], active :: Map Operand LiveInterval, registers :: Map Operand Operand}

  setAvailable a (AllocatorState _ b c) = AllocatorState a b c

  setActive a (AllocatorState b _ c) = AllocatorState b a c

  setRegisters a (AllocatorState b c _) = AllocatorState b c a

  type AllocatorStateMonad = State AllocatorState

  registerNumber (Scheduler.Register _ a) = a

  readWrote (MCInstruction (OpcodeCondition ARMMov _) [a, b]) = ([b], [a])
  readWrote (MCInstruction (OpcodeCondition ARMAdd _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMSub _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMMul _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMBl _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMAnd _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMOrr _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMEor _) [a, b, c]) = ([b, c], [a])
  readWrote (MCInstruction (OpcodeCondition ARMBl _) [a]) = ([a], [Scheduler.Register Physical 0])

  analyzeMachineCode a = ((filter isRegister ((fst . readWrote) a)), (filter isRegister ((snd . readWrote) a)))
    where
      isRegister (Scheduler.Register _ _) = True
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
    put (AnalyzerState (anc analyzedWrote + 1) (table analyzedWrote))

  analyzeOperands :: (Integer -> LiveInterval -> LiveInterval) -> [Operand] -> AnalyzerStateMonad ()
  analyzeOperands a [] = return ()

  analyzeOperands b (a:as) = do
    got <- get
    let analyzed = execState (analyzeOperand b a) got
    put analyzed
    analyzeOperands b as

  analyzeOperand :: (Integer -> LiveInterval -> LiveInterval) -> Operand -> AnalyzerStateMonad ()
  analyzeOperand b a = do
    got <- get
    if Map.notMember a (table got) then do
      let newTable = Map.insert a (b (anc got) (LiveInterval (-1) (-1))) (table got)
      put (AnalyzerState (anc got) newTable)
    else do
      let oldValue = (table got) Map.! a
      let newValue = (b (anc got) oldValue)
      let newTable = Map.insert a (whichValue oldValue newValue) (table got)
      put (AnalyzerState (anc got) newTable)
    where
      whichValue a b
        | liveFrom a < liveFrom b = a
        | otherwise = b

  analyze a = (Map.toList . table) (execState (analyzeOperandsPairs (analyzeMachineCodes a)) (AnalyzerState 0 Map.empty))

  allocateLiveIntervals :: [(Operand, LiveInterval)] -> AllocatorStateMonad ()
  allocateLiveIntervals [] = return ()

  allocateLiveIntervals (a:as) = do
    got <- get
    let allocated = execState (allocateLiveInterval a) got
    put allocated
    allocateLiveIntervals as

  allocateLiveInterval :: (Operand, LiveInterval) -> AllocatorStateMonad ()
  allocateLiveInterval a@(b@(Scheduler.Register Virtual _), c) = do
    got <- get
    let expired = execState (expireIntervals a ((sortBy compareLiveTo . Map.toList . active) got)) got
    if ((length . available) expired) == 0 then do
      return ()
    else do
      let newAvailable = (tail . available) expired
      let newActive = Map.insert b c (active expired)
      let newRegisters = Map.insert b (Scheduler.Register Physical ((head . available) expired)) (registers expired)
      put ((setAvailable newAvailable . setActive newActive . setRegisters newRegisters) expired)

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

  allocateMachineCodes a = operands
    where
      toPhysical = registers (execState (allocateLiveIntervals ((sortBy compareLiveFrom . analyze) a)) (AllocatorState [0..12] Map.empty Map.empty))
      operands = map instruction a
      operand b@(Scheduler.Register _ _) = toPhysical Map.! b
      operand b = b
      instruction (MCInstruction b c) = MCInstruction b (map operand c)
      instruction b = b

  allocate = map allocateMachineCodes
