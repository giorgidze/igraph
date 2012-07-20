{-# LANGUAGE ForeignFunctionInterface, PatternGuards, TupleSections #-}

module Data.IGraph.Internal where

import Data.IGraph.Basics
import Data.IGraph.Types

--import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

--import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

--import Data.List
import Data.IORef
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types


nodeToId' :: Graph d a -> a -> Int
nodeToId' (G g) n
  | Just i <- Map.lookup n (graphNodeToId g) = i
  | otherwise = error $ "nodeToId': Graph node/ID mismatch."

idToNode' :: Graph d a -> Int -> a
idToNode' (G g) i
  | Just n <- Map.lookup i (graphIdToNode g) = n
  | otherwise = error $ "idToNode': Graph ID/node mismatch, ID = " ++ show i

edgeIdToEdge :: Graph d a -> Int -> Edge d a
edgeIdToEdge g i
  | i < 0 || i >= Set.size es = error ("edgeIdToEdge: Index " ++ show i ++ " out of bound.")
  | otherwise                 = Set.toList es !! i
 where
  es = edges g

--------------------------------------------------------------------------------
-- Vector conversion

listToVector :: (Integral a) => [a] -> IO Vector
listToVector as = do
  vp <- c_igraph_vector_create (fromIntegral (length as))
  sizeRef <- newIORef (0 :: Int)
  forListM_ as $ \a -> do
      size <- readIORef sizeRef
      c_igraph_vector_set vp (fromIntegral size) (fromIntegral a)
      modifyIORef sizeRef (+1)
  fvp <- newForeignPtr c_igraph_vector_destroy vp
  return $ Vector fvp
  
vectorToList :: Vector -> IO [Double]
vectorToList (Vector fvp) = withForeignPtr fvp $ \vp -> do
  len <- c_igraph_vector_length vp
  let go :: [Double] -> CLong -> IO [Double]
      go acc 0 = return acc
      go acc i = do e <- c_igraph_vector_get vp (i - 1)
                    go (realToFrac e : acc) (i - 1)
  go [] len

{-
vectorPtrToList :: VectorPtrPtr -> IO [[Double]]
vectorPtrToList fvp = withForeignPtr fvp $ \vp -> do
  len <- c_igraph_vector_ptr_length vp
  let go :: [[Double]] -> CLong -> IO [[Double]]
      go acc 0 = return acc
      go acc i = do e <- c_igraph_vector_ptr_get vp (i - 1)
                    efp <- newForeignPtr c_igraph_vector_destroy e
                    v <- vectorToList efp
                    go (v : acc) (i - 1)
  go [] len
-}

edgesToVector :: Graph d a -> IO Vector
edgesToVector g@(G g') =
  listToVector $ Set.foldr (\e r -> toId (edgeFrom e) : toId (edgeTo e) : r) [] (edges g)
 where
  toId n | Just i <- Map.lookup n (graphNodeToId g') = i
         | otherwise = error "edgesToVector: Graph node/ID mismatch."

vectorToEdges :: Graph d a -> Vector -> IO [Edge d a]
vectorToEdges g@(G _) v = do
  l <- vectorToList v
  return $ map (edgeIdToEdge g . round) l

vectorToVertices :: Graph d a -> Vector -> IO [a]
vectorToVertices g@(G _) v = do
  fmap (map (idToNode' g . round)) (vectorToList v)


--------------------------------------------------------------------------------
-- Ptr stuff

withVector :: Vector -> (VectorPtr -> IO a) -> IO a
withVector (Vector fvp) = withForeignPtr fvp

withGraph :: Graph d a -> (GraphPtr -> IO res) -> IO (res, Graph d a)
withGraph g@(G g') io
  | Just fp <- graphForeignPtr g' = fmap (,g) (withForeignPtr fp io)
  | otherwise                     = do
    v <- edgesToVector g
    withVector v $ \vp -> do
    gp <- c_igraph_create vp (if isDirected g then 1 else 0)
    fp <- newForeignPtr c_igraph_destroy gp
    res <- withForeignPtr fp io
    return (res, G g'{ graphForeignPtr = Just fp })

withGraph_ :: Graph d a -> (GraphPtr -> IO res) -> IO res
withGraph_ g io = fmap fst $ withGraph g io


--------------------------------------------------------------------------------
-- Foreign imports

foreign import ccall "c_igraph_create"                    c_igraph_create                     :: VectorPtr -> CInt -> IO GraphPtr
foreign import ccall "&c_igraph_destroy"                  c_igraph_destroy                    :: FunPtr (GraphPtr  -> IO ())

foreign import ccall "c_igraph_vector_create"             c_igraph_vector_create              :: CLong  -> IO VectorPtr
foreign import ccall "&c_igraph_vector_destroy"           c_igraph_vector_destroy             :: FunPtr (VectorPtr -> IO ())
--foreign import ccall "c_igraph_vector_ptr_destroy"        c_igraph_vector_ptr_destroy         :: VectorPtrPtr -> IO ()

foreign import ccall "igraph_vector_set"                  c_igraph_vector_set                 :: VectorPtr -> CLong -> CDouble -> IO ()
foreign import ccall "igraph_vector_e"                    c_igraph_vector_get                 :: VectorPtr -> CLong -> IO CDouble
foreign import ccall "igraph_vector_size"                 c_igraph_vector_length              :: VectorPtr -> IO CLong
--foreign import ccall "igraph_vector_ptr_e"                c_igraph_vector_ptr_get             :: VectorPtrPtr -> CLong -> IO VectorPtr
--foreign import ccall "igraph_vector_ptr_size"             c_igraph_vector_ptr_length          :: VectorPtrPtr -> IO CLong


--------------------------------------------------------------------------------
-- Helper Functions

forListM_ :: [a] -> (a -> IO b) -> IO ()
forListM_ []       _ = return ()
forListM_ (a : as) f = (f a) >> (forListM_ as f)

-- forListM :: [a] -> (a -> IO b) -> IO [b]
-- forListM = go []
--   where
--   go :: [b] -> [a] -> (a -> IO b) -> IO [b]
--   go acc [] _       = return (reverse acc)
--   go acc (a : as) f = f a >>= \b -> go (b : acc) as f
