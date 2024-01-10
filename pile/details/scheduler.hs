module Scheduler where
  import Selector
  import Syntax

  data Operand =
    Register Integer |
    Immediate Integer deriving (Show, Eq)

  data MachineCode =
    MCInstruction OpcodeCondition [Operand] |
    MCSymbol String deriving (Show, Eq)

  opcodeCondition (Opcode a) = a

  scheduleGraph a = map toInstruction zipInstructions
    where
      opcodeNodes = filter isMachineCode (nodes a)
      isMachineCode (Node _ (BasicBlock _) _) = True
      isMachineCode (Node _ (Opcode _) _) = True
      isMachineCode _ = False
      nodeOperands b = map ((nodes a !!) . fromIntegral . fromNode) (filter ((isOperand . nodeID) b) (edges a))
      isOperand b (Edge _ c _) = b == c
      isOperandType (Selector.Register) = True
      isOperandType (Constant) = True
      isOperandType _ = False
      zipInstructions = zip opcodeNodes (map nodeOperands opcodeNodes)
      toInstruction ((Node _ (BasicBlock b) _), _) = MCSymbol b
      toInstruction (b@(Node _ (Opcode _) _), c) = MCInstruction (toOpcode b) (map toOperand (filter (isOperandType . nodeType) c))
      toOpcode b = (opcodeCondition . nodeType) b
      toOperand (Node _ Selector.Register [(_, (Just (IntegerValue a)))]) = Scheduler.Register a
      toOperand (Node _ Constant [(_, (Just (IntegerValue a)))]) = Immediate a

  schedule = map scheduleGraph
