module Optimizer where
  import Scheduler
  import Selector hiding (Register)
  import Syntax

  subStackPointer (MCInstruction (OpcodeCondition ARMSub _) [_, Register Physical 13, _]) = True
  subStackPointer _ = False

  replaceMachineCodes a [] = a

  replaceMachineCodes a (b@(MCInstruction (OpcodeCondition ARMStr _) [c, d, e]):f@(MCInstruction (OpcodeCondition ARMLdr _) [g, h, i]):j)
    | (c == g && d == h && e == i) = replaceMachineCodes (a ++ [b]) j
    | otherwise = replaceMachineCodes (a ++ [b]) (f:j)

  replaceMachineCodes a (b@(MCInstruction (OpcodeCondition ARMMov _) [c, d]):j)
    | c == d = replaceMachineCodes a j
    | otherwise = replaceMachineCodes (a ++ [b]) j

  replaceMachineCodes a (b@(MCInstruction (OpcodeCondition c d) [e, f, g]):h)
    | (c == ARMAdd || c == ARMSub) && g == Immediate 0 = replaceMachineCodes (a ++ replaceMachineCodes [] [MCInstruction (OpcodeCondition ARMMov d) [e, f]]) h
    | otherwise = replaceMachineCodes (a ++ [b]) h

  replaceMachineCodes a (b:bs) = replaceMachineCodes (a ++ [b]) bs

  optimizeStackPointers a = ((fst . machineCode) a) ++ [MCInstruction (OpcodeCondition ARMSub Nothing) [Register Physical 13, Register Physical 13, Immediate stackOffset]] ++ (filter (not . subStackPointer) ((snd . machineCode) a))
    where
      immediateValue (MCInstruction (OpcodeCondition _ _) [_, _, Immediate b]) = b
      stackOffset = (sum . map immediateValue . filter subStackPointer) a

  optimize = map (replaceMachineCodes [] . optimizeStackPointers)
