module Emitter where
  import Data.Char (toLower)
  import Data.List (intercalate)
  import Scheduler
  import Selector hiding (Label, Register)
  import Syntax

  emitNodeValue (IntegerValue a) = show a
  emitNodeValue (FloatingValue a) = show a

  emitMCSymbolScope MCGlobal a = ".global " ++ a ++ "\n"
  emitMCSymbolScope MCLocal _ = ""

  emitMachineCode (MCInstruction a@(OpcodeCondition b c) d) = "  " ++ emitOpcodeCondition a ++ " " ++ emitOperands
    where
      emitARM e = (drop 3 . map toLower . show) e
      emitOpcodeCondition (OpcodeCondition e Nothing) = emitARM e
      emitOpcodeCondition (OpcodeCondition e (Just f)) = emitARM e ++ emitARM f
      emitOperand (Label e) = "#" ++ e
      emitOperand (Register _ e) = "r" ++ show e
      emitOperand (Immediate e) = "#" ++ show e
      emitOperands
        | b == ARMLdr || b == ARMStr = ((emitOperand . head) d) ++ ", [" ++ (intercalate ", " ((map emitOperand . tail) d)) ++ "]"
        | otherwise = (intercalate ", " (map emitOperand d))

  emitMachineCode (MCSymbol a b) = emitMCSymbolScope a b ++ b ++ ":"
  emitMachineCode (MCDirective a@(MCConstant _ _)) = "  ." ++ emitMCDirective a
  emitMachineCode (MCDirective a) = "." ++ emitMCDirective a

  emitMachineCodes = map (map emitMachineCode)

  emitMCDirective (MCConstant Halfword a) = "hword " ++ emitNodeValue a
  emitMCDirective (MCConstant a b) = (map toLower . show) a ++ " " ++ emitNodeValue b
  emitMCDirective a = (map toLower . show) a

  emit = (++) "\n" . intercalate "\n" . map (intercalate "\n") . emitMachineCodes
