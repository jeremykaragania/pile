module Scheduler where
  import Selector
  import Syntax

  type Opcode = ARMOpcode

  data Operand =
    Register Integer |
    Immediate Integer deriving (Show, Eq)

  data Instruction = Instruction Opcode [Operand] deriving (Show, Eq)

  instructions a = zip opcodeNodes (map nodeOperands opcodeNodes)
    where
      opcodeNodes = filter isOpcode (nodes a)
      isOpcode (Node _ (Opcode _) _ _) = True
      isOpcode _ = False
      nodeOperands b = map (nodes a !!) (map (fromIntegral . fromNode) (filter ((isOperand . nodeID) b) (edges a)))
      isOperand a (Edge _ b _) = a == b

  schedule a = map (instructions) a
