module Scheduler where
  import Selector
  import Syntax

  data Operand =
    Register Integer |
    Immediate Integer deriving (Show, Eq)

  data MachineCode =
    MCInstruction OpcodeCondition [Operand] deriving (Show, Eq)

  opcodeCondition (Opcode a) = a

  scheduleGraph a = map toInstruction zipInstructions
    where
      opcodeNodes = filter isOpcode (nodes a)
      isOpcode (Node _ (Opcode _) _ _) = True
      isOpcode _ = False
      nodeOperands b = map (nodes a !!) (map (fromIntegral . fromNode) (filter ((isOperand . nodeID) b) (edges a)))
      isOperand a (Edge _ b _) = a == b
      isOperandType (Selector.Register) = True
      isOperandType (Constant) = True
      isOperandType _ = False
      zipInstructions = zip opcodeNodes (map nodeOperands opcodeNodes)
      toInstruction (b, c) = MCInstruction (toOpcode b) (map toOperand (filter (isOperandType . nodeType) c))
      toOpcode b = (opcodeCondition . nodeType) b
      toOperand (Node _ Selector.Register [(_, (Just (IntegerValue a)))] _) = Scheduler.Register a
      toOperand (Node _ Constant [(_, (Just (IntegerValue a)))] _) = Immediate a

  schedule a = map (scheduleGraph) a
