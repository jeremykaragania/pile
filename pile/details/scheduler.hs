module Scheduler where
  import Selector
  import Syntax

  {-
    Operand is a type for the operand of an instruction. An Operand can either
    be a register with a type and a number; an integer immediate; an address;
    or a string label.
  -}
  data Operand =
    Reg RegType Integer |
    Immediate Integer |
    Address Integer |
    Label String deriving (Show, Eq, Ord)

  {-
    Directive is a type for an assembler directive. The directives here are
    part of the GNU assembler.
  -}
  data Directive =
    MCConstant MachineValueType NodeValue |
    Data |
    Text deriving (Show, Eq)

  {-
    MCGlobalType is a type for global machine codes types. An global machine
    code can either be a variable or a function.
  -}
  data MCGlobalType =
    MCVariable |
    MCFunction deriving (Show, Eq)

  {-
    MCSymbolScope is a type for the scope of machine code symbols. A machine
    code symbol can either be global or local.
  -}
  data MCSymbolScope =
    MCGlobal MCGlobalType |
    MCLocal deriving (Show, Eq)

  {-
    MachineCode is a type for an abstract machine code. A machine code can
    either be an instruction; a symbol; or a directive.
  -}
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
      isOperandType (Selector.Reg _) = True
      isOperandType (Constant) = True
      isOperandType (Selector.Label _) = True
      isOperandType _ = False
      zipInstructions = zip opcodeNodes (map nodeOperands opcodeNodes)
      toMachineCode ((Node _ (FunctionGlobal b) _), _) = [MCDirective Text, MCSymbol (MCGlobal MCFunction) b]
      toMachineCode ((Node _ (VariableGlobal b) c), _) = [MCDirective Data, MCSymbol (MCGlobal MCVariable) b] ++ map (\(d, Just e) -> MCDirective (MCConstant d e)) c
      toMachineCode ((Node _ (BasicBlock b) _), _) = [MCSymbol MCLocal b]
      toMachineCode (b@(Node _ (Opcode _) _), c) = [MCInstruction (toOpcode b) (map toOperand (filter (isOperandType . nodeType) c))]
      toOpcode b = (opcodeCondition . nodeType) b
      toOperand (Node _ (Selector.Reg b) [(_, (Just (IntegerValue c)))]) = Scheduler.Reg b c
      toOperand (Node _ Constant [(_, (Just (IntegerValue b)))]) = Immediate b
      toOperand (Node _ Constant [(_, (Just (FloatingValue 0.0)))]) = Immediate 0
      toOperand (Node _ (Selector.Label b) _) = Scheduler.Label b

  schedule = map scheduleGraph
