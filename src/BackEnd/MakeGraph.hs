module BackEnd.MakeGraph where

import Data.HashMap as HashMap hiding (map)
import Algebra.Graph.AdjacencyMap as AdjGraph
import BackEnd.FlowGraph
import BackEnd.Assem as Assem


-- transform instructions to contro flow graph
instrsToGraph :: [Assem.Instr] -> FlowGraph
instrsToGraph instrs = fGraph { nodes = map fst indexed,
                                assems = HashMap.fromList indexed }
  where indexed = zip [0..] instrs -- index instructions

        -- create a mapping from label to node (int)
        labelTable =
          foldl (\acc (i, instr) ->
                   case instr of
                     ILABEL _ lab -> insert lab i acc
                     otherwise -> acc ) HashMap.empty indexed
        newDef = foldl (\acc i -> HashMap.insert i [] acc) HashMap.empty [0..length instrs -1]
        newUse = newDef
        fGraph = instrsToGraph' indexed (newFlowGraph { use = newUse,
                                                        def = newDef } )

        instrsToGraph' :: [(Int, Assem.Instr)] -> FlowGraph -> FlowGraph
        instrsToGraph' [] fGraph = fGraph

        instrsToGraph' (curr@(index, instr@(IOPER _ dst src _)):rest) fGraph =
          instrsToGraph' rest $ fGraph { control = adjGraph', def = def', use = use' }
          where
            targets = map (\l -> labelTable ! l) (jump instr)
            adjGraph' =
              case targets of
                [] -> if length rest == 0
                      then control fGraph
                      else overlay (edge index (index+1)) (control fGraph)
                otherwise -> foldl (\acc t -> overlay (edge index t) acc) (control fGraph) targets 
            def' = insertWith (++) index dst (def fGraph)
            use' = insertWith (++) index src (use fGraph)

        instrsToGraph' (curr@(index, ILABEL _ _):rest) fGraph =
          instrsToGraph' rest $ fGraph { control = adjGraph'}
          where adjGraph' =
                  if length rest == 0
                  then control fGraph
                  else overlay (edge index (index+1)) (control fGraph)

        instrsToGraph' ((index, (IMOV _ (dst:_) (src:_))):rest) fGraph =
          instrsToGraph' rest $ fGraph { control = adjGraph',
                                         def = insertWith (++) index [dst] (def fGraph),
                                         use = insertWith (++) index [src] (use fGraph) }
          where adjGraph' =
                  if length rest == 0
                  then control fGraph
                  else overlay (edge index (index+1)) (control fGraph)
