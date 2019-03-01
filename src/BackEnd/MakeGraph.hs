module BackEnd.MakeGraph where
import qualified Data.List as List
import Data.HashMap as HashMap hiding (map)
import Algebra.Graph.AdjacencyMap as AdjGraph
import BackEnd.Munch as Munch
import BackEnd.FlowGraph
import BackEnd.Assem as Assem
import BackEnd.Instructions as ARM


{-
  Transform a list of Assem.Instr to a control flow graph.
-}

testInstrsToGraphFile file = do
  (out, _, _) <- testMunch file
  return $ instrsToGraph (concat out)

-- transform instructions to control flow graph
instrsToGraph :: [Assem.Instr] -> FlowGraph
instrsToGraph instrs = fGraph { nodes = map fst indexed,
                                assems = HashMap.fromList indexed,
                                fInitial = inits List.\\ [0..15]}
  where indexed = zip [0..] instrs -- index instructions

        inits = List.nub $ concat $ map Assem.assemReg instrs
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
        instrsToGraph' ((index, (IOPER oper dst' src' js)):rest) fGraph =
          instrsToGraph' rest $ fGraph { control = adjGraph', def = def', use = use' }
          where
            targets = concat $ map (\l -> if HashMap.member l labelTable
                                          then [labelTable ! l] else []) js
            adjGraph' =
              case targets of
                [] -> if length rest == 0
                      then control fGraph
                      else overlay (edge index (index+1)) (control fGraph)
                otherwise ->
                  case  oper of
                    ARM.BRANCH_ (ARM.BL _) _ -> overlay (edge index (index+1)) edgeToTargets
                    otherwise -> edgeToTargets
            def' = insertWith (++) index dst' (def fGraph)
            use' = insertWith (++) index src' (use fGraph)
            edgeToTargets = foldl (\acc t -> overlay (edge index t) acc) (control fGraph) targets 

        instrsToGraph' ((index, (ILABEL _ _)):rest) fGraph =
          instrsToGraph' rest $ fGraph { control = adjGraph'}
          where adjGraph' =
                  if length rest == 0
                  then control fGraph
                  else overlay (edge index (index+1)) (control fGraph)

        instrsToGraph' ((index, (IMOV _ (dst':_) (src':_))):rest) fGraph =
          instrsToGraph' rest $ fGraph { control = adjGraph',
                                         def = insertWith (++) index [dst'] (def fGraph),
                                         use = insertWith (++) index [src'] (use fGraph) }
          where adjGraph' =
                  if length rest == 0
                  then control fGraph
                  else overlay (edge index (index+1)) (control fGraph)

        -- capture impossible situtaion
        instrsToGraph' ((index, (IMOV _ _ _)):rest) fGraph = undefined

