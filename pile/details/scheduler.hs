module Scheduler where
  import Selector
  import Syntax

  data Operand =
    Register RegisterType Integer |
    Immediate Integer |
    Label String deriving (Show, Eq, Ord)

  data MachineCode =
    MCInstruction OpcodeCondition [Operand] |
    MCSymbol String deriving (Show, Eq)

  opcodeCondition (Opcode a) = a

  scheduleGraph a = map toMachineCode zipInstructions
    where
      opcodeNodes = filter isMachineCode (nodes a)
      isMachineCode (Node _ (BasicBlock _) _) = True
      isMachineCode (Node _ (Opcode _) _) = True
      isMachineCode _ = False
      nodeOperands b = map ((nodes a !!) . fromIntegral . fromNode) (filter ((isOperand . nodeID) b) (edges a))
      isOperand b (Edge _ c _) = b == c
      isOperandType (Selector.Register _) = True
      isOperandType (Constant) = True
      isOperandType (Selector.Label _) = True
      isOperandType _ = False
      zipInstructions = zip opcodeNodes (map nodeOperands opcodeNodes)
      toMachineCode ((Node _ (BasicBlock b) _), _) = MCSymbol b
      toMachineCode (b@(Node _ (Opcode _) _), c) = MCInstruction (toOpcode b) (map toOperand (filter (isOperandType . nodeType) c))
      toOpcode b = (opcodeCondition . nodeType) b
      toOperand (Node _ (Selector.Register b) [(_, (Just (IntegerValue c)))]) = Scheduler.Register b c
      toOperand (Node _ Constant [(_, (Just (IntegerValue a)))]) = Immediate a
      toOperand (Node _ (Selector.Label b) _) = Scheduler.Label b

  schedule = map scheduleGraph
