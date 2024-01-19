module Optimizer where
  import Scheduler
  import Selector
  import Syntax

  subStackPointer (MCInstruction (OpcodeCondition ARMSub _) [_, Scheduler.Register Physical 13, _]) = True
  subStackPointer _ = False

  replaceMachineCodes a [] = a

  replaceMachineCodes a (b@(MCInstruction (OpcodeCondition ARMStr _) [c, d, e]):f@(MCInstruction (OpcodeCondition ARMLdr _) [g, h, i]):j)
    | (c == g && d == h && e == i) = replaceMachineCodes (a ++ [b]) j
    | otherwise = replaceMachineCodes (a ++ [b]) (f:j)

  replaceMachineCodes a (b:bs) = replaceMachineCodes (a ++ [b]) bs

  optimizeStackPointers a = ((fst . machineCode) a) ++ [MCInstruction (OpcodeCondition ARMSub Nothing) [Scheduler.Register Physical 13, Scheduler.Register Physical 13, Immediate offset]] ++ (filter (not . subStackPointer) ((snd . machineCode) a))
    where
      machineCode (MCSymbol _:MCSymbol _:as) = (take 2 a, drop 2 a)
      machineCode _  = ([head a], tail a)
      immediateValue (MCInstruction (OpcodeCondition _ _) [_, _, Immediate a]) = a
      offset = (sum . map immediateValue . filter subStackPointer) a

  optimizeMachineCodes = map (optimizeStackPointers . replaceMachineCodes [])
