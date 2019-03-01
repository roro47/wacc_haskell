module BackEnd.FlowGraph where

import Data.HashMap as HashMap hiding (map)
import Algebra.Graph.AdjacencyMap as AdjGraph
import BackEnd.Assem as Assem
import BackEnd.Temp as Temp

{- Note
  ------------------------------------------------------------------------------
  Control Flow Graph constructed from assembly instructions generated from munch
  (instruction selection).

  The design is inspired by the book Modern Compiler Implementation in ML.
-}



{- Note
  ------------------------------------------------------------------------------

  FlowGraph contains a control flow graph, where each node is the index of an
  assembly instruction.
-}

data FlowGraph =
  FlowGraph { control :: AdjGraph.AdjacencyMap Int,  -- control flow graph
              def :: HashMap.Map Int [Temp.Temp],    -- node -> temps defined at that node
              use :: HashMap.Map Int [Temp.Temp],    -- node -> temps used at that node
              nodes :: [Int],                        -- list of nodes
              assems :: HashMap.Map Int Assem.Instr, -- map from index to assembly instruction
              fInitial :: [Temp.Temp] }              -- all temps used
  deriving (Show)

newFlowGraph :: FlowGraph
newFlowGraph = FlowGraph { control = AdjGraph.empty,
                           def = HashMap.empty,
                           use = HashMap.empty,
                           nodes = [],
                           assems = HashMap.empty,
                           fInitial = []}
