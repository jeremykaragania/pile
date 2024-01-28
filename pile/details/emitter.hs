module Emitter where
  import Data.Char (toLower)
  import Data.List (intercalate)
  import Scheduler
  import Selector hiding (Label, Register)
  import Syntax

  emitNodeValue (IntegerValue a) = show a
  emitNodeValue (FloatingValue a) = show a

  emitMachineCode (MCInstruction a@(OpcodeCondition b c) d) = "  " ++ emitOpcodeCondition a ++ " " ++ emitOperands
    where
      emitARM a = (drop 3 . map toLower . show) a
      emitOpcodeCondition (OpcodeCondition a Nothing) = emitARM a
      emitOpcodeCondition (OpcodeCondition a (Just b)) = emitARM a ++ emitARM b
      emitOperand (Label a) = "#" ++ a
      emitOperand (Register _ a) = "r" ++ show a
      emitOperand (Immediate a) = "#" ++ show a
      emitOperands
        | b == ARMLdr || b == ARMStr = ((emitOperand . head) d) ++ ", [" ++ (intercalate ", " ((map emitOperand . tail) d)) ++ "]"
        | otherwise = (intercalate ", " (map emitOperand d))

  emitMachineCode (MCSymbol a) = a ++ ":"

  emitMachineCode (a@(MCDirective _ b)) = "  ." ++ emitMCDirective a ++ " " ++ emitNodeValue b

  emitMachineCodes = map (map emitMachineCode)

  emitMCDirective (MCDirective Byte _) = "byte"
  emitMCDirective (MCDirective Halfword _) = "hword"
  emitMCDirective (MCDirective Word _) = "word"

  emit = intercalate "\n" . map (intercalate "\n") . emitMachineCodes
