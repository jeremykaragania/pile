module Optimizer where
  import Scheduler
  import Selector
  import Syntax

  subStackPointer (MCInstruction (OpcodeCondition ARMSub _) [_, Scheduler.Register 13, _]) = True
  subStackPointer _ = False

  replaceMachineCodes a [] = a

  replaceMachineCodes a (b@(MCInstruction (OpcodeCondition ARMStr _) [c, d, e]):f@(MCInstruction (OpcodeCondition ARMLdr _) [g, h, i]):j)
    | (c == g && d == h && e == i) = replaceMachineCodes (a ++ [b]) j
    | otherwise = replaceMachineCodes (a ++ [b]) (f:j)

  replaceMachineCodes a (b:c) = replaceMachineCodes (a ++ [b]) c

  optimizeStackPointers a = [MCInstruction (OpcodeCondition ARMSub Nothing) [Scheduler.Register 13, Scheduler.Register 13, Immediate offset]] ++ (filter (not . subStackPointer) a)
    where
      immediateValue (MCInstruction (OpcodeCondition _ _) [_, _, Immediate a]) = a
      offset = (sum . map immediateValue . filter subStackPointer) a

  optimizeMachineCodes = map (optimizeStackPointers . replaceMachineCodes [])
