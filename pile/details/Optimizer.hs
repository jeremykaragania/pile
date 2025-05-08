module Optimizer where
  import Scheduler
  import Selector hiding (Reg)
  import Syntax

  subStackPointer (MCInstruction (OpcodeCondition ARMSub _) [_, Reg (RegType IntegerReg PhysicalReg) 13, _]) = True
  subStackPointer _ = False

  replaceMachineCodes a [] = a

  replaceMachineCodes a (b@(MCInstruction (OpcodeCondition ARMStr _) [c, d, e]):f@(MCInstruction (OpcodeCondition ARMLdr _) [g, h, i]):j)
    | (c == g && d == h && e == i) = replaceMachineCodes (a ++ [b]) j
    | otherwise = replaceMachineCodes (a ++ [b]) (f:j)

  replaceMachineCodes a (b@(MCInstruction (OpcodeCondition ARMMov _) [c, d]):e@(MCInstruction (OpcodeCondition ARMMov _) [f, g]):h)
    | c == g && d == f = replaceMachineCodes (a ++ [b]) h
    | otherwise = replaceMachineCodes (a ++ [b]) (e:h)

  replaceMachineCodes a (b@(MCInstruction (OpcodeCondition ARMMov _) [c, d]):j)
    | c == d = replaceMachineCodes a j
    | otherwise = replaceMachineCodes (a ++ [b]) j

  replaceMachineCodes a (b@(MCInstruction (OpcodeCondition c d) e):f@(MCInstruction (OpcodeCondition g h) i):j)
    | (c == ARMPush && g == ARMPush) || (c == ARMPop && g == ARMPop) && d == h = replaceMachineCodes a (merged:j)
    | otherwise = replaceMachineCodes (a ++ [b]) (f:j)
    where
      merged = (MCInstruction (OpcodeCondition c d) (e ++ i))

  replaceMachineCodes a (b:bs) = replaceMachineCodes (a ++ [b]) bs

  optimize = map (replaceMachineCodes [])
