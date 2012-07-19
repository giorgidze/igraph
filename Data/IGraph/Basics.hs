{-# LANGUAGE PatternGuards #-}

-- | Module for basic `Graph' functions without access to C libraries
module Data.IGraph.Basics where

import qualified Data.HashMap.Strict as Map

import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

import Data.IGraph.Types

emptyGraph :: Gr d a => Graph d a
emptyGraph = G $ Graph {- 0 -} 0 Map.empty Map.empty Map.empty Nothing

fromList :: Gr d a => [(a,a)] -> Graph d a
fromList = foldr (\(a,b) -> insertEdge (toEdge a b)) -- . insertNode a . insertNode b)
                 emptyGraph


member :: a -> Graph d a -> Bool
member a (G g) = a `Map.member` graphNodeToId g

-- insertNode :: a -> Graph d a -> Graph d a
-- insertNode n (G g)
--   | n `member` (G g) = G g -- node already in g
--   | otherwise = G $
--     g { graphNodeNumber = i
--       , graphIdToNode   = Map.insert i n (graphIdToNode g)
--       , graphNodeToId   = Map.insert n i (graphNodeToId g)
--       , graphForeignPtr = Nothing
--       }
--  where
--   i = graphNodeNumber g + 1

deleteNode :: a -> Graph d a -> Graph d a
deleteNode n (G g) = G $
  case Map.lookup n (graphNodeToId g) of
       Just i  -> g { -- graphNodeNumber = graphNodeNumber g - 1
                      graphIdToNode   = Map.delete i (graphIdToNode g)
                    , graphNodeToId   = Map.delete n (graphNodeToId g)
                    , graphEdges      = Map.map (Set.delete i) $ Map.delete i (graphEdges g)
                    , graphForeignPtr = Nothing
                    }
       Nothing -> g -- node not in graph

insertEdge :: Edge d a -> Graph d a -> Graph d a
insertEdge e (G g)
  | e `Set.member` edges (G g) || f == t = G g -- edge already in g or invalid edge
  | otherwise = G $
    case (Map.lookup f (graphNodeToId g), Map.lookup t (graphNodeToId g)) of
         (Just fi, Just ti) -> insertEdge' (fi,  ti,  (G g))
         (Nothing, Just ti) -> insertEdge' (i+1, ti,  insertNode f (i+1) (G g))
         (Just fi, Nothing) -> insertEdge' (fi,  i+1, insertNode t (i+1) (G g))
         (Nothing, Nothing) -> insertEdge' (i+1, i+2, insertNode t (i+2) $ insertNode f (i+1) (G g))
 where
  (f,t) = (edgeFrom e, edgeTo e)
  i     = Map.size (graphIdToNode g)

  insertEdge' (fi, ti, (G g')) =
    g' { graphEdgeNumber = graphEdgeNumber g' + 1
       , graphEdges      = insertEdge'' fi ti (isDirected (G g')) (graphEdges g')
       , graphForeignPtr = Nothing
       }

  insertEdge'' f' t' False = insertEdge'' t' f' True
                           . insertEdge'' f' t' True
  insertEdge'' f' t' True  = Map.insertWith Set.union f' (Set.singleton t')

  insertNode :: a -> Int -> Graph d a -> Graph d a
  insertNode n ni (G g') = G $
    g' { graphIdToNode   = Map.insert ni n  (graphIdToNode g')
       , graphNodeToId   = Map.insert n  ni (graphNodeToId g') }

deleteEdge :: Edge d a -> Graph d a -> Graph d a
deleteEdge e (G g) = G $
  case (Map.lookup f (graphNodeToId g), Map.lookup t (graphNodeToId g)) of
       (Just fi, Just ti) -> g { graphEdgeNumber = graphEdgeNumber g - 1
                               , graphEdges      = deleteEdge' fi ti (isDirected (G g)) (graphEdges g)
                               , graphForeignPtr = Nothing
                               }
       _                  -> g -- not both nodes in graph
 where
  (f,t) = (edgeFrom e, edgeTo e)
  deleteEdge' f' t' False = deleteEdge' t' f' True
                          . deleteEdge' f' t' True
  deleteEdge' f' t' True  = Map.adjust (Set.delete t') f'

nodes :: Graph d a -> HashSet a
nodes (G g) = Set.fromList . Map.keys $ graphNodeToId g

edges :: Graph d a -> HashSet (Edge d a)
edges (G g) = Map.foldrWithKey (\f ts es -> Set.union (Set.map (mkEdge f) ts) es) Set.empty (graphEdges g)
 where
  mkEdge f t
    | Just fn <- Map.lookup f (graphIdToNode g)
    , Just tn <- Map.lookup t (graphIdToNode g)
    = toEdge fn tn
    | otherwise
    = error "edges: Graph node/ID mismatch."

neighbours :: a -> Graph d a -> HashSet a
neighbours n (G g)
  | Just i <- Map.lookup n (graphNodeToId g)
  = maybe Set.empty (Set.map mkNode) $ Map.lookup i (graphEdges g)
  | otherwise
  = Set.empty
 where
  mkNode i | Just n' <- Map.lookup i (graphIdToNode g) = n'
           | otherwise
           = error "neighbours: Graph node/ID mismatch."
