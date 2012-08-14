{-# LANGUAGE ForeignFunctionInterface, PatternGuards, TupleSections #-}

module Data.IGraph.Internal where

import Data.IGraph.Basics
import Data.IGraph.Types

--import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

--import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

import qualified Data.Map as M

import Control.Monad
import Control.Monad.State
import Data.IORef
import Foreign
import Foreign.C


nodeToId'' :: Graph d a -> a -> Int
nodeToId'' (G g) n
  | Just i <- Map.lookup n (graphNodeToId g) = i
  | otherwise = error $ "nodeToId': Graph node/ID mismatch."

idToNode'' :: Graph d a -> Int -> a
idToNode'' (G g) i
  | Just n <- Map.lookup i (graphIdToNode g) = n
  | otherwise = error $ "idToNode': Graph ID/node mismatch, ID = " ++ show i

edgeIdToEdge :: Graph d a -> Int -> Edge d a
edgeIdToEdge g i
  | i < 0 || i >= Set.size es = error ("edgeIdToEdge: Index " ++ show i ++ " out of bound.")
  | otherwise                 = Set.toList es !! i
 where
  es = edges' g


--------------------------------------------------------------------------------
-- IGraph

runIGraph :: Graph d a -> IGraph (Graph d a) r -> (r, Graph d a)
runIGraph g (IGraph ig) = runState ig g

evalIGraph :: Graph d a -> IGraph (Graph d a) r -> r
evalIGraph g (IGraph ig) = evalState ig g

execIGraph :: Graph d a -> IGraph (Graph d a) r -> Graph d a
execIGraph g (IGraph ig) = execState ig g

localGraph :: Graph d' a' -> IGraph (Graph d' a') r -> IGraph (Graph d a) r
localGraph g ig = return $ evalIGraph g ig

foreign import ccall "igraphhaskell_initialize"
  c_igraphhaskell_initialize :: IO CInt

initIGraph :: IO ()
initIGraph = {- (0 ==) -} (const ()) `fmap` c_igraphhaskell_initialize

runUnsafeIO :: (Graph d a -> IO (r, Graph d a)) -> IGraph (Graph d a) r
runUnsafeIO f = do
  g <- get
  (r,g') <- return $ unsafePerformIO $ initIGraph >> f g
  put g'
  return r


--------------------------------------------------------------------------------
-- Graphs

foreign import ccall "c_igraph_create"
  c_igraph_create :: VectorPtr -> CInt -> IO GraphPtr

{-
withGraph :: (GraphPtr -> IO res) -> IGraph (Graph d a) res
withGraph f = do
  io <- gets $ withGraph' `flip` f
  let (r, g) = unsafePerformIO io
  put g
  return r
-}

withGraph :: Graph d a -> (GraphPtr -> IO res) -> IO (res, Graph d a)
withGraph g@(G g') io
  | Just fp <- graphForeignPtr g' = fmap (,g) (withForeignPtr fp io)
  | otherwise                     = do
    v <- edgesToVector g
    withVector v $ \vp -> do
      gp  <- c_igraph_create vp (if isDirected g then 1 else 0)
      fp  <- newForeignPtr c_igraph_destroy gp
      res <- withForeignPtr fp io
      return (res, G g'{ graphForeignPtr = Just fp })

withGraph_ :: Graph d a -> (GraphPtr -> IO res) -> IO res
withGraph_ g io = fmap fst $ withGraph g io

setGraphPointer :: Graph d a -> GraphPtr -> IO (Graph d a)
setGraphPointer (G g) gp = do
  fp <- newForeignPtr c_igraph_destroy gp
  return $ G g{ graphForeignPtr = Just fp }

foreign import ccall "edges"
  c_igraph_edges :: GraphPtr -> IO VectorPtrPtr

subgraphFromPtr :: Graph d a    -- ^ original graph containing all informations
                                -- about node labels etc.
                -> GraphPtr     -- ^ new (sub)graph pointer
                -> IO (Graph d a)
subgraphFromPtr g@(G _) gp = do
  vpp     <- c_igraph_edges gp
  vp      <- newVectorPtr' vpp
  Just is <- getVertexIds' gp
  [l1,l2] <- vectorPtrToList vp
  let lookupM = M.fromList $ zip [0..] (map round is)
      orgId :: Int -> Int
      orgId i | Just o <- M.lookup i lookupM = o
              | otherwise = error $ "subgraphFromPtr: Invalid ID " ++ show i
      getNodes = map (idToNode'' g . orgId . round)
      ls = zip (getNodes l1) (getNodes l2)
  return $ fromList ls

--
-- Graph IDs
--

foreign import ccall "igraphhaskell_graph_set_vertex_ids"
  c_igraphhaskell_graph_set_vertex_ids :: GraphPtr -> IO ()

setVertexIds :: IGraph (Graph d a) ()
setVertexIds = runUnsafeIO $ \g -> do
  g' <- setVertexIds' g
  return ((),g')

setVertexIds' :: Graph d a -> IO (Graph d a)
setVertexIds' g = do
  withGraph_ g $ \gp -> do
    c_igraphhaskell_graph_set_vertex_ids gp
    setGraphPointer g gp

foreign import ccall "igraphhaskell_graph_get_vertex_ids"
  c_igraphhaskell_graph_get_vertex_ids :: GraphPtr -> VectorPtr -> IO CInt

getVertexIds :: Graph d a -> IO (Maybe [Double])
getVertexIds g = withGraph_ g getVertexIds'

getVertexIds' :: GraphPtr -> IO (Maybe [Double])
getVertexIds' gp = do
  v <- newVector 0
  s <- withVector v $ c_igraphhaskell_graph_get_vertex_ids gp
  if s == 0
     then Just `fmap` vectorToList v
     else return Nothing


--------------------------------------------------------------------------------
-- Vertex selectors

foreign import ccall "c_igraph_vs_create"
  c_igraph_vs_create :: IO VsPtr

foreign import ccall "&c_igraph_vs_destroy"
  c_igraph_vs_destroy :: FunPtr (VsPtr -> IO ())

newVs :: IO VsForeignPtr
newVs = do
  vsp <- c_igraph_vs_create
  fvp <- newForeignPtr c_igraph_vs_destroy vsp
  return $ VsF fvp

--applyVs :: VsIdent a -> VertexSelector a -> IO VsForeignPtr
--applyVs f (Vs vs) = vs f

withVs :: VertexSelector a -> (Graph d a) -> (VsPtr -> IO res) -> IO res
withVs (Vs vs) g f = do
  fvs <- vs (nodeToId' g)
  withVs' fvs f

withVs' :: VsForeignPtr -> (VsPtr -> IO res) -> IO res
withVs' (VsF fp) f = withForeignPtr fp f


--------------------------------------------------------------------------------
-- Vectors

newVector :: Int -> IO Vector
newVector s = do
  vp <- c_igraph_vector_create (fromIntegral s)
  newVector' vp

newVector' :: VectorPtr -> IO Vector
newVector' vp = do
  fvp <- newForeignPtr c_igraph_vector_destroy vp
  return $ Vector fvp

listToVector :: (Integral a) => [a] -> IO Vector
listToVector as = do
  v <- newVector (length as)
  withVector v $ \vp -> do
    sizeRef <- newIORef (0 :: Int)
    forListM_ as $ \a -> do
      size <- readIORef sizeRef
      c_igraph_vector_set vp (fromIntegral size) (fromIntegral a)
      modifyIORef sizeRef (+1)
  return v
  
vectorToList :: Vector -> IO [Double]
vectorToList (Vector fvp) = withForeignPtr fvp $ \vp -> do
  len <- c_igraph_vector_length vp
  let go :: [Double] -> CLong -> IO [Double]
      go acc 0 = return acc
      go acc i = do e <- c_igraph_vector_get vp (i - 1)
                    go (realToFrac e : acc) (i - 1)
  go [] len

newVectorPtr :: Int -> IO VectorP
newVectorPtr s = do
  vp <- c_igraph_vector_ptr_create (fromIntegral s)
  newVectorPtr' vp

newVectorPtr' :: VectorPtrPtr -> IO VectorP
newVectorPtr' vp = do
  fvp <- newForeignPtr c_igraph_vector_ptr_destroy vp
  return $ VectorP fvp

vectorPtrToList :: VectorP -> IO [[Double]]
vectorPtrToList (VectorP fvp) = withForeignPtr fvp $ \vp -> do
  len <- c_igraph_vector_ptr_length vp
  let go :: [[Double]] -> CLong -> IO [[Double]]
      go acc 0 = return acc
      go acc i = do e <- c_igraph_vector_ptr_get vp (i - 1)
                    efp <- newForeignPtr c_igraph_vector_destroy e
                    v <- vectorToList (Vector efp)
                    go (v : acc) (i - 1)
  go [] len

edgesToVector :: Graph d a -> IO Vector
edgesToVector g@(G g') =
  listToVector $ Set.foldr (\e r -> toId (edgeFrom e) : toId (edgeTo e) : r) [] (edges' g)
 where
  toId n | Just i <- Map.lookup n (graphNodeToId g') = i
         | otherwise = error "edgesToVector: Graph node/ID mismatch."

vectorToEdges :: Graph d a -> Vector -> IO [Edge d a]
vectorToEdges g@(G _) v = do
  l <- vectorToList v
  return $ map (edgeIdToEdge g . round) l

vectorToVertices :: Graph d a -> Vector -> IO [a]
vectorToVertices g@(G _) v = do
  fmap (map (idToNode'' g . round)) (vectorToList v)

vectorPtrToVertices :: Graph d a -> VectorP -> IO [[a]]
vectorPtrToVertices g@(G _) v = do
  fmap (map (map (idToNode'' g . round))) (vectorPtrToList v)

--------------------------------------------------------------------------------
-- Matrices

foreign import ccall "igraph_matrix_e"
  c_igraph_matrix_get     :: MatrixPtr -> CLong -> CLong -> IO CDouble
foreign import ccall "c_igraph_matrix_create"
  c_igraph_matrix_create  :: CLong -> CLong -> IO MatrixPtr
foreign import ccall "&c_igraph_matrix_destroy"
  c_igraph_matrix_destroy :: FunPtr (MatrixPtr -> IO ())
foreign import ccall "igraph_matrix_set"
  c_igraph_matrix_set     :: MatrixPtr -> CLong -> CLong -> CDouble -> IO ()
foreign import ccall "igraph_matrix_nrow"
  c_igraph_matrix_nrow    :: MatrixPtr -> IO CLong
foreign import ccall "igraph_matrix_ncol"
  c_igraph_matrix_ncol    :: MatrixPtr -> IO CLong
foreign import ccall "igraph_matrix_get_row"
  c_igraph_matrix_get_row :: MatrixPtr -> VectorPtr -> CLong -> IO CInt

newMatrix :: Int -> Int -> IO Matrix
newMatrix nrow ncol = do
  mp  <- c_igraph_matrix_create (fromIntegral nrow) (fromIntegral ncol)
  fmp <- newForeignPtr c_igraph_matrix_destroy mp
  return $ Matrix fmp

getMatrixValue :: Matrix -> Int -> Int -> IO Double
getMatrixValue (Matrix fmp) x y = withForeignPtr fmp $ \ mp -> do
  cd <- c_igraph_matrix_get mp (fromIntegral x) (fromIntegral y)
  return $ realToFrac cd

listToMatrix :: Integral a => [[a]] -> IO Matrix
listToMatrix l = do
  m <- newMatrix nrow ncol
  withMatrix m $ \mp ->
    -- fill the matrix
    forListM_ (zip [0..] l)     $ \(r,row) ->
      forListM_ (zip [0..] row) $ \(c,val) ->
        c_igraph_matrix_set mp r c (fromIntegral val)
  return m
 where
  nrow = maximum (map length l)
  ncol = length l

matrixToList :: Matrix -> IO [[Double]]
matrixToList m = withMatrix m $ \mp -> do
  nrow <- c_igraph_matrix_nrow mp
  ncol <- c_igraph_matrix_ncol mp
  forM [0..nrow-1] $ \r -> do
    v  <- newVector (fromIntegral ncol)
    _e <- withVector v $ \vp ->
      c_igraph_matrix_get_row mp vp r
    vectorToList v
    
  


--------------------------------------------------------------------------------
-- Ptr stuff

withMatrix :: Matrix -> (MatrixPtr -> IO a) -> IO a
withMatrix (Matrix fmp) = withForeignPtr fmp

withVector :: Vector -> (VectorPtr -> IO a) -> IO a
withVector (Vector fvp) = withForeignPtr fvp

withVectorPtr :: VectorP -> (VectorPtrPtr -> IO a) -> IO a
withVectorPtr (VectorP fvp) = withForeignPtr fvp


--------------------------------------------------------------------------------
-- Foreign imports

foreign import ccall "&c_igraph_destroy"                  c_igraph_destroy                    :: FunPtr (GraphPtr  -> IO ())

foreign import ccall "c_igraph_vector_create"             c_igraph_vector_create              :: CLong -> IO VectorPtr
foreign import ccall "&c_igraph_vector_destroy"           c_igraph_vector_destroy             :: FunPtr (VectorPtr -> IO ())
foreign import ccall "c_igraph_vector_ptr_create"         c_igraph_vector_ptr_create          :: CLong -> IO VectorPtrPtr
foreign import ccall "&c_igraph_vector_ptr_destroy"       c_igraph_vector_ptr_destroy         :: FunPtr (VectorPtrPtr -> IO ())

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
