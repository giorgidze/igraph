{-# LANGUAGE ForeignFunctionInterface, PatternGuards #-}

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


--------------------------------------------------------------------------------
-- Vector conversion

listToVector :: (Integral a) => [a] -> IO VectorPtr
listToVector as = do
  vector <- c_igraph_vector_create (2 * fromIntegral (length as))
  sizeRef <- newIORef (0 :: Int)
  forListM_ as $ \a -> do
      size <- readIORef sizeRef
      c_igraph_vector_set vector (fromIntegral size) (fromIntegral a)
      modifyIORef sizeRef (+1)
  return vector

vectorToList :: VectorPtr -> IO [Double]
vectorToList vector = do
  len <- c_igraph_vector_length vector
  let go :: [Double] -> CLong -> IO [Double]
      go acc 0 = return acc
      go acc i = do e <- c_igraph_vector_get vector (i - 1)
                    go (realToFrac e : acc) (i - 1)
  go [] len

vectorPtrToList :: VectorPtrPtr -> IO [[Double]]
vectorPtrToList vectorPtr = do
  len <- c_igraph_vector_ptr_length vectorPtr
  let go :: [[Double]] -> CLong -> IO [[Double]]
      go acc 0 = return acc
      go acc i = do e <- c_igraph_vector_ptr_get vectorPtr (i - 1)
                    v <- vectorToList e
                    go (v : acc) (i - 1)
  go [] len


--------------------------------------------------------------------------------
-- Graph <-> Ptr stuff

withGraph :: Graph d a -> (GraphPtr -> IO res) -> IO (res, Graph d a)
withGraph g@(G g') io
  | Just fp <- graphForeignPtr g' = withVector' fp
  | otherwise                     = do
    vp <- edgesToVector g
    gp <- c_igraph_create vp (if isDirected g then 1 else 0)
    fp <- newForeignPtr c_igraph_destroy gp
    withVector' fp
 where
  withVector' fp = do
    res <- withForeignPtr fp io
    return (res, G g'{ graphForeignPtr = Just fp })


edgesToVector :: Graph d a -> IO VectorPtr
edgesToVector g@(G g') =
  listToVector $ Set.foldr (\e r -> toId (edgeFrom e) : toId (edgeTo e) : r) [] (edges g)
 where
  toId n | Just i <- Map.lookup n (graphNodeToId g') = i
         | otherwise = error "edgesToVector: Graph node/ID mismatch."


--------------------------------------------------------------------------------
-- Foreign imports

foreign import ccall "c_igraph_create"                    c_igraph_create                     :: VectorPtr -> CInt -> IO GraphPtr
foreign import ccall "&c_igraph_destroy"                  c_igraph_destroy                    :: FunPtr (GraphPtr  -> IO ())

foreign import ccall "c_igraph_vector_create"             c_igraph_vector_create              :: CLong  -> IO VectorPtr
--foreign import ccall "c_igraph_vector_destroy"            c_igraph_vector_destroy             :: VectorPtr -> IO ()
--foreign import ccall "c_igraph_vector_ptr_destroy"        c_igraph_vector_ptr_destroy         :: VectorPtrPtr -> IO ()

foreign import ccall "igraph_vector_set"                  c_igraph_vector_set                 :: VectorPtr -> CLong -> CDouble -> IO ()
foreign import ccall "igraph_vector_e"                    c_igraph_vector_get                 :: VectorPtr -> CLong -> IO CDouble
foreign import ccall "igraph_vector_size"                 c_igraph_vector_length              :: VectorPtr -> IO CLong
foreign import ccall "igraph_vector_ptr_e"                c_igraph_vector_ptr_get             :: VectorPtrPtr -> CLong -> IO VectorPtr
foreign import ccall "igraph_vector_ptr_size"             c_igraph_vector_ptr_length          :: VectorPtrPtr -> IO CLong


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
