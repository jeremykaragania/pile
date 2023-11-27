module Selector where
  import Control.Monad.State
  import Data.Int
  import Syntax

  data NodeType =
    EntryToken |
    BasicBlock |
    Register |
    Constant |
    Opcode ARMOpcode deriving (Show, Eq)

  data NodeValue = 
    Word Int32 |
    Halfword Int16 |
    Byte Int8 deriving (Show, Eq)

  data Node = Node {nodeId :: Integer, nodeType :: NodeType, nodeValues :: [NodeValue], nodeOrder :: Maybe Integer} deriving (Show, Eq)

  data Edge = Edge {fromNode :: Integer, toNode :: Integer, result :: Integer} deriving (Show, Eq)

  data Graph = Graph {nodes :: [Node], edges :: [Edge]} deriving (Show, Eq)

  data SelectorState = SelectorState {graph :: Graph}

  type SelectorStateMonad = State SelectorState
