module Emitter where
  import Data.Char (toLower)
  import Data.List (intercalate, sort)
  import Scheduler
  import Selector hiding (Label, Reg)
  import Syntax

  emitNodeValue (IntegerValue a) = show a
  emitNodeValue (FloatingValue a) = show a

  emitMCSymbolScope (MCGlobal _) a = ".global " ++ a ++ "\n"
  emitMCSymbolScope MCLocal _ = ""

  emitMCSymbolType (MCGlobal MCFunction) = "function"
  emitMCSymbolType (MCGlobal MCVariable) = "object"
  emitMCSymbolType _ = "notype"

  emitMachineCode (MCInstruction a@(OpcodeCondition b _) d) = "  " ++ emitOpcodeCondition a ++ " " ++ emitOperands
    where
      emitARM e = (drop 3 . map toLower . show) e
      emitOpcodeCondition (OpcodeCondition e Nothing) = emitARM e
      emitOpcodeCondition (OpcodeCondition e (Just f)) = emitARM e ++ emitARM f
      emitOperand (Label e) = e
      emitOperand (Reg (RegType IntegerReg PhysicalReg) 15) = "pc"
      emitOperand (Reg (RegType IntegerReg PhysicalReg) 14) = "lr"
      emitOperand (Reg (RegType IntegerReg PhysicalReg) 13) = "sp"
      emitOperand (Reg (RegType IntegerReg PhysicalReg) 12) = "ip"
      emitOperand (Reg (RegType IntegerReg PhysicalReg) 11) = "fp"
      emitOperand (Reg (RegType IntegerReg PhysicalReg) e) = "r" ++ show e
      emitOperand (Reg (RegType SingleReg PhysicalReg) e) = "s" ++ show e
      emitOperand (Reg (RegType DoubleReg PhysicalReg) e) = "d" ++ show e
      emitOperand (Immediate e) = "#" ++ show e
      emitOperands
        | b == ARMLdr || b == ARMStr = ((emitOperand . head) d) ++ ", [" ++ (intercalate ", " ((map emitOperand . tail) d)) ++ "]"
        | b == ARMPush || b == ARMPop = "{" ++ (intercalate ", " (map emitOperand (sort d))) ++ "}"
        | otherwise = (intercalate ", " (map emitOperand d))

  emitMachineCode (MCSymbol a b) = ".type " ++ b ++ ", %" ++ emitMCSymbolType a ++ "\n" ++ emitMCSymbolScope a b ++ b ++ ":"
  emitMachineCode (MCDirective a@(MCConstant _ _)) = "  ." ++ emitMCDirective a
  emitMachineCode (MCDirective a) = "." ++ emitMCDirective a

  emitMachineCodes = map (map emitMachineCode)

  emitMCDirective (MCConstant Halfword a) = "hword " ++ emitNodeValue a
  emitMCDirective (MCConstant a b) = (map toLower . show) a ++ " " ++ emitNodeValue b
  emitMCDirective a = (map toLower . show) a

  emit = flip (++) "\n" . intercalate "\n" . map (intercalate "\n") . emitMachineCodes
