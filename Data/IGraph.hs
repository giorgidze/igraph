{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE  ForeignFunctionInterface #-}

module Data.IGraph
  ( Graph (..), G, Gr (..), D, U
  , emptyGraph
    -- * Graph modification
  , insertNode, deleteNode
  , insertEdge, deleteEdge
    -- * Graph information
  , nodes, edges, neighbours
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

import Data.Hashable

import Foreign.ForeignPtr

data Void

-- | The internal graph representation wrapped into a GADT to carry around the
-- @Gr d a@ class constraint.
data Graph d a where
  G :: Gr d a => G d a -> Graph d a

-- | The internal graph representation.
data G d a = Graph { graphNodeNumber        :: !(Int)
                   , graphEdgeNumber        :: !(Int)
                   , graphIdToNode          :: !(HashMap Int a)
                   , graphNodeToId          :: !(HashMap a Int)
                   , graphEdges             :: !(HashMap Int (HashSet Int))
                   , graphForeignPtr        :: !(Maybe (ForeignPtr Void))
                   }

-- | Graph class. Minimal definition: @data Edge d a@ with `Hashable' and `Eq'
-- instances, `toEdge', `edgeFrom', `edgeTo', `isDirected'
class (Eq a, Hashable a, Eq (Edge d a), Hashable (Edge d a)) => Gr d a where

  data Edge d a
  toEdge     :: a -> a -> Edge d a
  edgeFrom   :: Edge d a -> a
  edgeTo     :: Edge d a -> a
  isDirected :: Graph d a -> Bool

-- | Undirected graph
data U

instance (Eq a, Hashable a) => Gr U a where
  isDirected _ = False
  data Edge U a = U_Edge a a
  toEdge = U_Edge
  edgeFrom (U_Edge a _) = a
  edgeTo   (U_Edge _ b) = b

instance Eq a => Eq (Edge U a) where
  (U_Edge a b) == (U_Edge c d) = (a,b) == (c,d) || (a,b) == (d,c)

instance Hashable a => Hashable (Edge U a) where
  hash (U_Edge a b) = hash (a,b) + hash (b,a) -- to make sure (a,b) receives the same hash as (b,a)

instance Show a => Show (Edge U a) where
  show (U_Edge a b) = "Edge U {" ++ show a ++ " <-> " ++ show b ++ "}"

-- | Directed graph
data D

instance (Eq a, Hashable a) => Gr D a where
  isDirected _ = True
  data Edge D a = D_Edge a a deriving Eq
  toEdge = D_Edge
  edgeFrom (D_Edge a _) = a
  edgeTo   (D_Edge _ b) = b

instance Hashable a => Hashable (Edge D a) where
  hash (D_Edge a b) = hash (a,b)

instance Show a => Show (Edge D a) where
  show (D_Edge a b) = "Edge D {" ++ show a ++ " -> " ++ show b ++ "}"


emptyGraph :: Gr d a => Graph d a
emptyGraph = G $ Graph 0 0 Map.empty Map.empty Map.empty Nothing


insertNode :: a -> Graph d a -> Graph d a
insertNode n (G g) = G $
  g { graphNodeNumber = i
    , graphIdToNode   = Map.insert i n (graphIdToNode g)
    , graphNodeToId   = Map.insert n i (graphNodeToId g)
    , graphForeignPtr = Nothing
    }
 where
  i = graphNodeNumber g + 1

deleteNode :: a -> Graph d a -> Graph d a
deleteNode n (G g) = G $
  case Map.lookup n (graphNodeToId g) of
       Just i  -> g { graphNodeNumber = graphNodeNumber g - 1
                    , graphIdToNode   = Map.delete i (graphIdToNode g)
                    , graphNodeToId   = Map.delete n (graphNodeToId g)
                    , graphEdges      = Map.map (Set.delete i) $ Map.delete i (graphEdges g)
                    , graphForeignPtr = Nothing
                    }
       Nothing -> g -- node not in graph

insertEdge :: Edge d a -> Graph d a -> Graph d a
insertEdge e (G g) = G $
  case (Map.lookup f (graphNodeToId g), Map.lookup t (graphNodeToId g)) of
       (Just fi, Just ti) -> g { graphEdgeNumber = graphEdgeNumber g + 1
                               , graphEdges      = insertEdge' fi ti (isDirected (G g)) (graphEdges g)
                               , graphForeignPtr = Nothing
                               }
       _                  -> g -- not both nodes in graph
 where
  (f,t) = (edgeFrom e, edgeTo e)
  insertEdge' f' t' False = insertEdge' t' f' True
                          . insertEdge' f' t' True
  insertEdge' f' t' True  = Map.insertWith Set.union f' (Set.singleton t')

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
    = error "Error in `edges': Graph node/ID mismatch."

neighbours :: a -> Graph d a -> HashSet a
neighbours n (G g)
  | Just i <- Map.lookup n (graphNodeToId g)
  = maybe Set.empty (Set.map mkNode) $ Map.lookup i (graphEdges g)
  | otherwise
  = Set.empty
 where
  mkNode i | Just n' <- Map.lookup i (graphIdToNode g) = n'
           | otherwise
           = error "Error in `neighbours': Graph node/ID mismatch."

-- import Data.List

-- import qualified Data.ByteString.Char8 as BS
-- 
-- import Data.IORef
-- import Foreign.Ptr
-- import Foreign.ForeignPtr
-- import Foreign.C.Types
-- 
-- import System.IO.Unsafe (unsafePerformIO)

-- data Void
-- 
-- type GraphPtr     = Ptr Void
-- type VectorPtr    = Ptr Void
-- type VectorPtrPtr = Ptr Void
-- type Node         = Int
-- 
-- data Edge = Edge { edgeFr :: !Node
--                  , edgeTo :: !Node
--                  } deriving (Eq, Ord, Show)
-- 
-- instance Hashable Edge where
--   hash (Edge fr to) = hash (fr,to)
-- 
-- data Graph a = Graph { graphNodeNumber  :: !(Int)
--                      , graphEdgeNumber  :: !(Int)
--                      , graphEdges       :: !(HashSet Edge)
--                      , graphNodeToLabel :: !(HashMap Node a)
--                      , graphLabelToNode :: !(HashMap a Node)
--                      , graphForeignPtr  :: !(ForeignPtr Void)
--                      }
-- 
-- make :: (Eq a, Hashable a) => [(a,a)] -> Graph a
-- make edgesWithNodeLabels =
--   Graph { graphNodeNumber  = grNodeNumber
--         , graphEdgeNumber  = grEdgeNumber
--         , graphEdges       = grEdges
--         , graphNodeToLabel = grNodeToLabel
--         , graphLabelToNode = grLabelToNode
--         , graphForeignPtr  = grForeignPtr
--         }
--   where
--   es            = Set.fromList edgesWithNodeLabels
--   grEdgeNumber  = Set.size es
--   nodeLabels    = Set.union (Set.map fst es) (Set.map snd es)
--   grNodeNumber  = Set.size nodeLabels
--   grNodeToLabel = Map.fromList (zip [0 .. ] (Set.toList nodeLabels))
--   grLabelToNode = Map.fromList [ (l,n) | (n,l) <- Map.toList grNodeToLabel ]
--   grEdges       = Set.fromList [ Edge (grLabelToNode ! fr) (grLabelToNode ! to)
--                                | (fr,to) <- Set.toList es ]
--   grForeignPtr  = unsafePerformIO $ do  vector <- edgeSetToVector grEdges
--                                         grPtr <- c_igraph_create vector
--                                         c_igraph_vector_destroy vector
--                                         newForeignPtr c_igraph_destroy grPtr
-- 
-- makeFromFile :: FilePath -> IO (Graph Int)
-- makeFromFile fp = do
--   bs <- BS.readFile fp
-- 
--   let errorMessage = error ("Unable to import the graph file '" ++ fp ++ "'\n")
--   let func (frb : tob : _) = case (BS.readInt frb,BS.readInt tob) of
--                                (Just (fr,_),Just(to,_)) -> (fr,to)
--                                _                        -> errorMessage
--       func _               = errorMessage
-- 
--   return $! make $ map func $ map (BS.words) (BS.lines bs)
-- 
-- nodeNumber :: Graph a -> Int
-- nodeNumber = graphNodeNumber
-- 
-- edgeNumber :: Graph a -> Int
-- edgeNumber = graphEdgeNumber
-- 
-- edges :: Graph a -> [(Node,Node)]
-- edges = map (\e -> (edgeFr e, edgeTo e)). Set.toList . graphEdges
-- 
-- labels :: Graph a -> [a]
-- labels = Map.keys . graphLabelToNode
-- 
-- member :: (Eq a, Hashable a) => Graph a -> a -> Bool
-- member gr a = Map.member a (graphLabelToNode gr)
-- 
-- listToVector :: (Integral a) => [a] -> IO VectorPtr
-- listToVector as = do
--   vector <- c_igraph_vector_create (2 * fromIntegral (length as))
--   sizeRef <- newIORef (0 :: Int)
--   forListM_ as $ \a -> do
--       size <- readIORef sizeRef
--       c_igraph_vector_set vector (fromIntegral size) (fromIntegral a)
--       modifyIORef sizeRef (+1)
--   return vector
-- 
-- edgeListToVector :: [Edge] -> IO VectorPtr
-- edgeListToVector = listToVector . concatMap (\ed -> [edgeFr ed,edgeTo ed])
-- 
-- edgeSetToVector :: HashSet Edge -> IO VectorPtr
-- edgeSetToVector = edgeListToVector . Set.toList
-- 
-- vectorToList :: VectorPtr -> IO [Double]
-- vectorToList vector = do
--   len <- c_igraph_vector_length vector
--   let go :: [Double] -> CLong -> IO [Double]
--       go acc 0 = return acc
--       go acc i = do e <- c_igraph_vector_get vector (i - 1)
--                     go (realToFrac e : acc) (i - 1)
--   go [] len
-- 
-- vectorPtrToList :: VectorPtrPtr -> IO [[Double]]
-- vectorPtrToList vectorPtr = do
--   len <- c_igraph_vector_ptr_length vectorPtr
--   let go :: [[Double]] -> CLong -> IO [[Double]]
--       go acc 0 = return acc
--       go acc i = do e <- c_igraph_vector_ptr_get vectorPtr (i - 1)
--                     v <- vectorToList e
--                     go (v : acc) (i - 1)
--   go [] len
-- 
-- betweennessNodes :: Graph a -> [(Node,Double)]
-- betweennessNodes gr = unsafePerformIO $ withForeignPtr (graphForeignPtr gr) $ \grPtr -> do
--   vector <- c_igraph_betweenness grPtr
--   list   <- vectorToList vector
--   c_igraph_vector_destroy vector
--   return (zip [0 .. ] list)
-- 
-- betweenness :: Graph a -> [(a,Double)]
-- betweenness gr = [ ((graphNodeToLabel gr) ! node,s) | (node,s) <- betweennessNodes gr ]
-- 
-- eigenvectorCentralityNodes :: Graph a -> [(Node,Double)]
-- eigenvectorCentralityNodes gr = unsafePerformIO $ withForeignPtr (graphForeignPtr gr) $ \grPtr -> do
--   vector <- c_igraph_eigenvector_centrality grPtr
--   list   <- vectorToList vector
--   c_igraph_vector_destroy vector
--   return (zip [0 .. ] list)
-- 
-- eigenvectorCentrality :: Graph a -> [(a,Double)]
-- eigenvectorCentrality gr = [ ((graphNodeToLabel gr) ! node,s) | (node,s) <- eigenvectorCentralityNodes gr ]
-- 
-- clusterNodes :: Graph a -> [[Node]]
-- clusterNodes gr = unsafePerformIO $ withForeignPtr (graphForeignPtr gr) $ \grPtr -> do
--   vector <- c_igraph_clusters grPtr
--   list   <- vectorToList vector
--   c_igraph_vector_destroy vector
--   return $ [ [ n | (i,n) <- zip list [0 .. ], i == ci ] | ci <- nub list ]
-- 
-- cluster :: Graph a -> [[a]]
-- cluster gr = [ [ (graphNodeToLabel gr) ! node | node <- nodes ] | nodes <- clusterNodes gr ]
-- 
-- closenessInNode :: Graph a -> Node -> Double
-- closenessInNode gr nd = unsafePerformIO $ withForeignPtr (graphForeignPtr gr) $ \grPtr -> do
--   vector <- c_igraph_closeness_in grPtr (fromIntegral nd)
--   list   <- vectorToList vector
--   c_igraph_vector_destroy vector
--   return (head list)
-- 
-- closenessIn :: (Eq a, Hashable a) => Graph a -> a -> Double
-- closenessIn gr a = closenessInNode gr ((graphLabelToNode gr) ! a)
-- 
-- closenessIns :: (Eq a, Hashable a) => Graph a -> [a] -> [Double]
-- closenessIns gr = map (closenessIn gr)
-- 
-- closenessOutNode :: Graph a -> Node -> Double
-- closenessOutNode gr nd = unsafePerformIO $ withForeignPtr (graphForeignPtr gr) $ \grPtr -> do
--   vector <- c_igraph_closeness_out grPtr (fromIntegral nd)
--   list   <- vectorToList vector
--   c_igraph_vector_destroy vector
--   return (head list)
-- 
-- closenessOut :: (Eq a, Hashable a) => Graph a -> a -> Double
-- closenessOut gr a = closenessOutNode gr ((graphLabelToNode gr) ! a)
-- 
-- closenessOuts :: (Eq a, Hashable a) => Graph a -> [a] -> [Double]
-- closenessOuts gr = map (closenessOut gr)
-- 
-- shortestPathsInNode :: Graph a -> Node -> [[Node]]
-- shortestPathsInNode gr nd = unsafePerformIO $ withForeignPtr (graphForeignPtr gr) $ \grPtr -> do
--   vectorPtr <- c_igraph_get_shortest_paths_in grPtr (fromIntegral nd)
--   lists   <- vectorPtrToList vectorPtr
--   c_igraph_vector_ptr_destroy vectorPtr
--   return (map (map round) lists)
-- 
-- shortestPathsIn :: (Eq a, Hashable a) => Graph a -> a -> [[a]]
-- shortestPathsIn gr a = [ [ (graphNodeToLabel gr) ! n | n <- nodes ]
--                        | nodes <- shortestPathsInNode gr ((graphLabelToNode gr) ! a)
--                        , not (null nodes) ]
-- 
-- foreign import ccall "c_igraph_create"                    c_igraph_create                     :: VectorPtr -> IO GraphPtr
-- foreign import ccall "&c_igraph_destroy"                  c_igraph_destroy                    :: FunPtr (GraphPtr  -> IO ())
-- foreign import ccall "c_igraph_vector_create"             c_igraph_vector_create              :: CLong  -> IO VectorPtr
-- foreign import ccall "c_igraph_vector_destroy"            c_igraph_vector_destroy             :: VectorPtr -> IO ()
-- foreign import ccall "c_igraph_vector_ptr_destroy"        c_igraph_vector_ptr_destroy         :: VectorPtrPtr -> IO ()
-- foreign import ccall "igraph_vector_set"                  c_igraph_vector_set                 :: VectorPtr -> CLong -> CDouble -> IO ()
-- foreign import ccall "igraph_vector_e"                    c_igraph_vector_get                 :: VectorPtr -> CLong -> IO CDouble
-- foreign import ccall "igraph_vector_size"                 c_igraph_vector_length              :: VectorPtr -> IO CLong
-- foreign import ccall "igraph_vector_ptr_e"                c_igraph_vector_ptr_get             :: VectorPtrPtr -> CLong -> IO VectorPtr
-- foreign import ccall "igraph_vector_ptr_size"             c_igraph_vector_ptr_length          :: VectorPtrPtr -> IO CLong
-- foreign import ccall "c_igraph_betweenness"               c_igraph_betweenness                :: GraphPtr -> IO VectorPtr
-- foreign import ccall "c_igraph_closeness_in"              c_igraph_closeness_in               :: GraphPtr -> CInt -> IO VectorPtr
-- foreign import ccall "c_igraph_closeness_out"             c_igraph_closeness_out              :: GraphPtr -> CInt -> IO VectorPtr
-- foreign import ccall "c_igraph_get_shortest_paths_in"     c_igraph_get_shortest_paths_in      :: GraphPtr -> CInt -> IO VectorPtrPtr
-- foreign import ccall "c_igraph_eigenvector_centrality"    c_igraph_eigenvector_centrality     :: GraphPtr -> IO VectorPtr
-- foreign import ccall "c_igraph_clusters"                  c_igraph_clusters                   :: GraphPtr -> IO VectorPtr
-- 
-- -- Helper Functions
-- 
-- forListM_ :: [a] -> (a -> IO b) -> IO ()
-- forListM_ []       _ = return ()
-- forListM_ (a : as) f = (f a) >> (forListM_ as f)
-- 
-- -- forListM :: [a] -> (a -> IO b) -> IO [b]
-- -- forListM = go []
-- --   where
-- --   go :: [b] -> [a] -> (a -> IO b) -> IO [b]
-- --   go acc [] _       = return (reverse acc)
-- --   go acc (a : as) f = f a >>= \b -> go (b : acc) as f
