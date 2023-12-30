module Scheduler where
  import Selector
  import Syntax

  type Opcode = ARMOpcode

  data Operand =
    Register Integer |
    Immediate Integer deriving (Show, Eq)

  data Instruction = Instruction Opcode [Operand] deriving (Show, Eq)

  opcode (Opcode a) = a

  scheduleInstructions a = map toInstruction zipInstructions
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
      toInstruction (b, c) = Instruction (toOpcode b) (map toOperand (filter (isOperandType . nodeType) c))
      toOpcode b = (opcode . nodeType) b
      toOperand (Node _ Selector.Register [(_, (Just (IntegerValue a)))] _) = Scheduler.Register a
      toOperand (Node _ Constant [(_, (Just (IntegerValue a)))] _) = Immediate a

  schedule a = map (scheduleInstructions) a
