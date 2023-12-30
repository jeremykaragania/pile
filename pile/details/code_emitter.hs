module CodeEmitter where
  import Data.Char (toLower)
  import Data.List (intercalate)
  import Scheduler
  import Syntax

  emitInstruction (Instruction a b) = emitOpcode ++ " " ++ emitOperands
    where
      emitOpcode = (drop 3 . map toLower . show) a
      emitOperand (Register a) = "r" ++ show a
      emitOperand (Immediate a) = "#" ++ show a
      emitOperands
        | a == ARMLdr || a == ARMStr = ((emitOperand . head) b) ++ " [" ++ (intercalate ", " ((map emitOperand . tail) b)) ++ "]"
        | otherwise = (intercalate ", " (map emitOperand b))

  emitInstructions = map emitInstruction
