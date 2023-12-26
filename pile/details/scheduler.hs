module Scheduler where
  import Syntax

  type Opcode = ARMOpcode

  data Operand =
    Register Integer |
    Immediate Integer deriving (Show, Eq)

  data Instruction = Instruction Opcode [Operand] deriving (Show, Eq)
