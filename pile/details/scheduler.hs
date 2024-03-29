module Scheduler where
  import Selector
  import Syntax

  data Operand =
    Register RegisterType Integer |
    Immediate Integer |
    Address Integer |
    Label String deriving (Show, Eq, Ord)

  data Directive =
    MCConstant MachineValueType NodeValue |
    Data |
    Text deriving (Show, Eq)

  data MCGlobalType =
    MCVariable |
    MCFunction deriving (Show, Eq)

  data MCSymbolScope =
    MCGlobal MCGlobalType |
    MCLocal deriving (Show, Eq)

  data MachineCode =
    MCInstruction OpcodeCondition [Operand] |
    MCSymbol MCSymbolScope String |
    MCDirective Directive deriving (Show, Eq)

  opcodeCondition (Opcode a) = a

  machineCode a@(MCSymbol _ _:MCSymbol _ _:_) = (take 2 a, drop 2 a)
  machineCode a = ([head a], tail a)

  scheduleGraph a = (concat . map toMachineCode) zipInstructions
    where
      opcodeNodes = filter isMachineCode (nodes a)
      isMachineCode (Node _ (FunctionGlobal _) _) = True
      isMachineCode (Node _ (VariableGlobal _) _) = True
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
      toMachineCode ((Node _ (FunctionGlobal b) _), _) = [MCDirective Text, MCSymbol (MCGlobal MCFunction) b]
      toMachineCode ((Node _ (VariableGlobal b) c), _) = [MCDirective Data, MCSymbol (MCGlobal MCVariable) b] ++ map (\(d, Just e) -> MCDirective (MCConstant d e)) c
      toMachineCode ((Node _ (BasicBlock b) _), _) = [MCSymbol MCLocal b]
      toMachineCode (b@(Node _ (Opcode _) _), c) = [MCInstruction (toOpcode b) (map toOperand (filter (isOperandType . nodeType) c))]
      toOpcode b = (opcodeCondition . nodeType) b
      toOperand (Node _ (Selector.Register b) [(_, (Just (IntegerValue c)))]) = Scheduler.Register b c
      toOperand (Node _ Constant [(_, (Just (IntegerValue b)))]) = Immediate b
      toOperand (Node _ Constant [(_, (Just (FloatingValue 0.0)))]) = Immediate 0
      toOperand (Node _ (Selector.Label b) _) = Scheduler.Label b

  schedule = map scheduleGraph
