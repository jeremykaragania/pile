module CodeEmitter where
  import Data.Char (toLower)
  import Data.List (intercalate)
  import Scheduler
  import Selector
  import Syntax

  emitMachineCode (MCInstruction a@(OpcodeCondition b c) d) = "  " ++ emitOpcodeCondition a ++ " " ++ emitOperands
    where
      emitARM a = (drop 3 . map toLower . show) a
      emitOpcodeCondition (OpcodeCondition a Nothing) = emitARM a
      emitOpcodeCondition (OpcodeCondition a (Just b)) = emitARM a ++ emitARM b
      emitOperand (Scheduler.Label a) = a
      emitOperand (Scheduler.Register a) = "r" ++ show a
      emitOperand (Immediate a) = "#" ++ show a
      emitOperands
        | b == ARMLdr || b == ARMStr = ((emitOperand . head) d) ++ " [" ++ (intercalate ", " ((map emitOperand . tail) d)) ++ "]"
        | otherwise = (intercalate ", " (map emitOperand d))

  emitMachineCode (MCSymbol a) = a ++ ":"

  emitMachineCodes = map (map emitMachineCode)

  emit = intercalate "\n" . map (intercalate "\n") . emitMachineCodes
