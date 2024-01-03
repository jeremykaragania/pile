module Optimizer where
  import Scheduler
  import Selector
  import Syntax

  replaceMachineCodes a [] = a

  replaceMachineCodes a (b@(MCInstruction (OpcodeCondition ARMStr _) [c, d, e]):f@(MCInstruction (OpcodeCondition ARMLdr _) [g, h, i]):j)
    | (c == g && d == h && e == i) = replaceMachineCodes (a ++ [b]) j
    | otherwise = replaceMachineCodes (a ++ [b]) (f:j)

  replaceMachineCodes a (b:c) = replaceMachineCodes (a ++ [b]) c

  optimizeMachineCodes = map (replaceMachineCodes [])
