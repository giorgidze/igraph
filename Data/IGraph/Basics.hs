{-# LANGUAGE PatternGuards #-}

-- | Module for basic `Graph' functions without access to C libraries
module Data.IGraph.Basics where

import qualified Data.HashMap.Strict as Map

import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

import Data.List
import Control.Monad.State

import Data.IGraph.Types

emptyGraph :: E d a => Graph d a
emptyGraph = G $ Graph 0 0 Map.empty Map.empty Set.empty Nothing

fromList :: E d a => [(a,a)] -> Graph d a
fromList = foldl' (\g (a,b) -> insertEdge' (toEdge a b) g) emptyGraph

numberOfNodes :: IGraph (Graph d a) Int
numberOfNodes = gets numberOfNodes'

numberOfNodes' :: Graph d a -> Int
numberOfNodes' (G g) = graphNodeNumber g

numberOfEdges :: IGraph (Graph d a) Int
numberOfEdges = gets numberOfEdges'

numberOfEdges' :: Graph d a -> Int
numberOfEdges' (G g) = graphEdgeNumber g

member :: a -> IGraph (Graph d a) Bool
member a = gets $ member' a

member' :: a -> Graph d a -> Bool
member' a (G g) = a `Map.member` graphNodeToId g

nodeToId :: a -> IGraph (Graph d a) (Maybe Int)
nodeToId a = gets $ nodeToId' `flip` a

nodeToId' :: Graph d a -> a -> Maybe Int
nodeToId' (G g) n = Map.lookup n (graphNodeToId g)

idToNode :: Int -> IGraph (Graph d a) (Maybe a)
idToNode i = gets $ idToNode' `flip` i

idToNode' :: Graph d a -> Int -> Maybe a
idToNode' (G g) i = Map.lookup i (graphIdToNode g)

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

deleteNode :: a -> IGraph (Graph d a) ()
deleteNode n = modify $ deleteNode' n

deleteNode' :: a -> Graph d a -> Graph d a
deleteNode' n (G g) = G $
  case Map.lookup n (graphNodeToId g) of
       Just i  -> g { graphNodeNumber = graphNodeNumber g - 1
                    , graphIdToNode   = Map.delete i (graphIdToNode g)
                    , graphNodeToId   = Map.delete n (graphNodeToId g)
                    , graphEdges      = Set.filter (\e -> edgeFrom e /= n && edgeTo e /= n) (graphEdges g)
                    , graphForeignPtr = Nothing
                    }
       Nothing -> g -- node not in graph

insertEdge :: Edge d a -> IGraph (Graph d a) ()
insertEdge e = modify $ insertEdge' e

insertEdge' :: Edge d a -> Graph d a -> Graph d a
insertEdge' e (G g)
  | e `Set.member` edges' (G g) || f == t = G g -- edge already in g or invalid edge
  | otherwise = G $
    case (Map.member f (graphNodeToId g), Map.member t (graphNodeToId g)) of
         (True,  True)  -> insertEdge'' ((G g))
         (False, True)  -> insertEdge'' (insertNode f i (G g))
         (True,  False) -> insertEdge'' (insertNode t i (G g))
         (False, False) -> insertEdge'' (insertNode t (i+1) $ insertNode f i (G g))
 where
  (f,t) = (edgeFrom e, edgeTo e)
  i     = Map.size (graphIdToNode g)

  insertEdge'' (G g') =
    g' { graphEdgeNumber = graphEdgeNumber g' + 1
       , graphEdges      = Set.insert e (graphEdges g')
       , graphForeignPtr = Nothing
       }

  insertNode :: a -> Int -> Graph d a -> Graph d a
  insertNode n ni (G g') = G $
    g' { graphNodeNumber = graphNodeNumber g' + 1
       , graphIdToNode   = Map.insert ni n  (graphIdToNode g')
       , graphNodeToId   = Map.insert n  ni (graphNodeToId g') }

deleteEdge :: Edge d a -> IGraph (Graph d a) ()
deleteEdge e = modify $ deleteEdge' e

deleteEdge' :: Edge d a -> Graph d a -> Graph d a
deleteEdge' e (G g)
  | Set.member e (graphEdges g) = deleteNodes $ G $
    g { graphEdges      = Set.delete e (graphEdges g)
      , graphEdgeNumber = graphEdgeNumber g - 1
      , graphForeignPtr = Nothing
      }
  | otherwise = G g
 where
  (f,t) = (edgeFrom e, edgeTo e)
  deleteNodes g' =
    let delF = if Set.null (neighbours' f g') then deleteNode' f else id
        delT = if Set.null (neighbours' t g') then deleteNode' t else id
     in delT . delF $ g'

nodes :: IGraph (Graph d a) (HashSet a)
nodes = gets nodes'

nodes' :: Graph d a -> HashSet a
nodes' (G g) = Set.fromList $ Map.keys $ graphNodeToId g

edges :: IGraph (Graph d a) (HashSet (Edge d a))
edges = gets edges'

edges' :: Graph d a -> HashSet (Edge d a)
edges' (G g) = graphEdges g

neighbours :: a -> IGraph (Graph d a) (HashSet a)
neighbours n = gets $ neighbours' n

neighbours' :: a -> Graph d a -> HashSet a
neighbours' n g@(G _) =
  Set.foldr neighbours'' Set.empty (edges' g)
 where
  neighbours'' e r
    | edgeFrom e == n                       = Set.insert (edgeTo   e) r
    | edgeTo   e == n && not (isDirected g) = Set.insert (edgeFrom e) r
    | otherwise                             = r
