module Data.IGraph
  ( Graph (..), G, Gr (..), D, U
    -- * Construction
  , emptyGraph, fromList
  , insertEdge, deleteEdge, deleteNode
    -- * Query
  , null, member, nodes, edges, neighbours
  ) where

import Data.IGraph.Basics
import Data.IGraph.Internal
import Data.IGraph.Types

import Foreign.ForeignPtr

-- makeFromFile :: FilePath -> IO (Graph d a)
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

{-
betweennessNodes :: Graph d a -> [(Node,Double)]
betweennessNodes gr = unsafePerformIO $ withForeignPtr (graphForeignPtr gr) $ \grPtr -> do
  vector <- c_igraph_betweenness grPtr
  list   <- vectorToList vector
  c_igraph_vector_destroy vector
  return (zip [0 .. ] list)
-}

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
