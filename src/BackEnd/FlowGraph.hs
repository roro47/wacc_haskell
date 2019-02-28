module BackEnd.FlowGraph where

import Data.HashMap as HashMap hiding (map)
import Algebra.Graph.AdjacencyMap as AdjGraph
import BackEnd.Assem as Assem
import BackEnd.Temp as Temp

data FlowGraph =
  FlowGraph { control :: AdjGraph.AdjacencyMap Int,
              def :: HashMap.Map Int [Temp.Temp],
              use :: HashMap.Map Int [Temp.Temp],
              nodes :: [Int],
              assems :: HashMap.Map Int Assem.Instr,
              fInitial :: [Temp.Temp] }
  deriving (Show)


newFlowGraph :: FlowGraph
newFlowGraph = FlowGraph { control = AdjGraph.empty,
                           def = HashMap.empty,
                           use = HashMap.empty,
                           nodes = [],
                           assems = HashMap.empty,
                           fInitial = []}
                             
data JUMP = JustJump [String]
          | JumpFall [String]
          deriving (Show)
