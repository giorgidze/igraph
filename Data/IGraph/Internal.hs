{-# OPTIONS -fno-warn-orphans #-}

module Data.IGraph.Internal where

import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

import Control.Monad
import Data.List
import Data.IORef
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

import Data.IGraph.Types
import Data.IGraph.Internal.Constants

nodeToId'' :: Graph d a -> a -> Int
nodeToId'' (G g) n = case Map.lookup n (graphNodeToId g) of
  Just i  -> i
  Nothing -> error "nodeToId': Graph node/ID mismatch."

idToNode'' :: Graph d a -> Int -> a
idToNode'' (G g) i = case Map.lookup i (graphIdToNode g) of
  Just n  -> n
  Nothing -> error ("idToNode': Graph ID/node mismatch, ID = " ++ show i)

edgeIdToEdge :: Graph d a -> Int -> Edge d a
edgeIdToEdge g i
  | i < 0 || i >= Set.size es = error ("edgeIdToEdge: Index " ++ show i ++ " out of bound.")
  | otherwise                 = Set.toList es !! i
 where
  es = edges g

--------------------------------------------------------------------------------
-- Graphs

foreign import ccall "c_igraph_create"
  c_igraph_create :: VectorPtr -> CInt -> IO (Ptr Void)

buildForeignGraph :: Graph d a -> Graph d a
buildForeignGraph g@(G gr) = G (gr {graphForeignPtr = unsafePerformIO io})
  where
  io :: IO (ForeignPtr Void)
  io = do v <- edgesToVector g
          withVector v $ \vp -> do
            gp  <- c_igraph_create vp (if isDirected g then 1 else 0)
            newForeignPtr c_igraph_destroy gp

withGraph :: Graph d a -> (Ptr Void -> IO res) -> IO res
withGraph (G gr) = withForeignPtr (graphForeignPtr gr)

setGraphPointer :: Graph d a -> Ptr Void -> IO (Graph d a)
setGraphPointer (G g) gp = do
  fp <- newForeignPtr c_igraph_destroy gp
  return $ G g{ graphForeignPtr = fp }

withWeights :: Graph (Weighted d) a -> (VectorPtr -> IO res) -> IO res
withWeights g io = do
  v <- listToVector $ map getWeight (Set.toList (edges g))
  withVector v io

withOptionalWeights :: Graph d a -> (VectorPtr -> IO res) -> IO res
withOptionalWeights g@(G _) io = do
  let mws = getWeights g
  case mws of
       Nothing -> io nullPtr
       Just ws -> listToVector ws >>= flip withVector io

foreign import ccall "edges"
  c_igraph_edges :: GraphPtr -> IO VectorPtrPtr

--------------------------------------------------------------------------------
-- (orphan) graph instances

instance Show a => Show (Graph U a) where
  show (G g) = show (graphEdges g)

instance Show a => Show (Graph D a) where
  show (G g) = show (graphEdges g)

instance Eq (Graph U a) where
  (G g1) == (G g2) = graphEdges g1 == graphEdges g2

instance Eq (Graph D a) where
  (G g1) == (G g2) = graphEdges g1 == graphEdges g2

instance Show (Edge d a) => Show (Graph (Weighted d) a) where
  show (G g) = show (graphEdges g)

instance Eq (Edge d a) => Eq (Graph (Weighted d) a) where
  (G g1) == (G g2) = graphEdges g1 == graphEdges g2

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


foreign import ccall "igraph_vs_all"
  c_igraph_vs_all :: VsPtr -> IO CInt

foreign import ccall "igraph_vs_adj"
  c_igraph_vs_adj :: VsPtr -> CInt -> CInt -> IO CInt

foreign import ccall "igraph_vs_nonadj"
  c_igraph_vs_nonadj :: VsPtr -> CInt -> CInt -> IO CInt

foreign import ccall "igraph_vs_none"
  c_igraph_vs_none :: VsPtr -> IO CInt

foreign import ccall "igraph_vs_1"
  c_igraph_vs_1 :: VsPtr -> CInt -> IO CInt

foreign import ccall "igraph_vs_vector"
  c_igraph_vs_vector :: VsPtr -> VectorPtr -> IO CInt

{-
foreign import ccall "igraph_vs_seq"
  c_igraph_vs_seq :: VsPtr -> CInt -> CInt -> IO CInt
-}

withVs :: VertexSelector a -> Graph d a -> (VsPtr -> IO res) -> IO res
withVs vs g f = do
  fvs <- newVs
  -- bind to C vertex selector pointer
  _e <- withVs' fvs $ \vsp ->
    case vs of
         VsAll      -> c_igraph_vs_all    vsp
         VsNone     -> c_igraph_vs_none   vsp
         VsAdj    a -> c_igraph_vs_adj    vsp (ident a) (fromIntegral $ fromEnum Out)
         VsNonAdj a -> c_igraph_vs_nonadj vsp (ident a) (fromIntegral $ fromEnum Out)
         Vs1      a -> c_igraph_vs_1      vsp (ident a)
         VsList   l -> do
           v <- listToVector (map ident l)
           withVector v $ c_igraph_vs_vector vsp
  --fvs <- vs (nodeToId' g)
  withVs' fvs f
 where
  ident a = fromIntegral (nodeToId'' g a) :: CInt

withVs' :: VsForeignPtr -> (VsPtr -> IO res) -> IO res
withVs' (VsF fp) = withForeignPtr fp


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
-- Vectors

foreign import ccall "c_igraph_vector_create"             c_igraph_vector_create              :: CLong -> IO VectorPtr
foreign import ccall "&c_igraph_vector_destroy"           c_igraph_vector_destroy             :: FunPtr (VectorPtr -> IO ())

newVector :: Int -> IO Vector
newVector s = do
  vp <- c_igraph_vector_create (fromIntegral s)
  newVector' vp

newVector' :: VectorPtr -> IO Vector
newVector' vp = do
  fvp <- newForeignPtr c_igraph_vector_destroy vp
  return $ Vector fvp

foreign import ccall "igraph_vector_set"                  c_igraph_vector_set                 :: VectorPtr -> CLong -> CDouble -> IO ()
foreign import ccall "igraph_vector_e"                    c_igraph_vector_get                 :: VectorPtr -> CLong -> IO CDouble
foreign import ccall "igraph_vector_size"                 c_igraph_vector_length              :: VectorPtr -> IO CLong

vectorToList :: Vector -> IO [Double]
vectorToList (Vector fvp) = withForeignPtr fvp $ \vp -> do
  len <- c_igraph_vector_length vp
  let go :: [Double] -> CLong -> IO [Double]
      go acc 0 = return acc
      go acc i = do e <- c_igraph_vector_get vp (i - 1)
                    go (realToFrac e : acc) (i - 1)
  go [] len

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


--------------------------------------------------------------------------------
-- VectorPtr

foreign import ccall "c_igraph_vector_ptr_create"         c_igraph_vector_ptr_create          :: CLong -> IO VectorPtrPtr
foreign import ccall "&c_igraph_vector_ptr_destroy"       c_igraph_vector_ptr_destroy         :: FunPtr (VectorPtrPtr -> IO ())

newVectorPtr :: Int -> IO VectorP
newVectorPtr s = do
  vp <- c_igraph_vector_ptr_create (fromIntegral s)
  newVectorPtr' vp

newVectorPtr' :: VectorPtrPtr -> IO VectorP
newVectorPtr' vp = do
  fvp <- newForeignPtr c_igraph_vector_ptr_destroy vp
  return $ VectorP fvp

foreign import ccall "igraph_vector_ptr_e"                c_igraph_vector_ptr_get             :: VectorPtrPtr -> CLong -> IO VectorPtr
foreign import ccall "igraph_vector_ptr_size"             c_igraph_vector_ptr_length          :: VectorPtrPtr -> IO CLong

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
  listToVector $ Set.foldr (\e r -> toId (edgeFrom e) : toId (edgeTo e) : r) [] (edges g)
 where
  toId n = case Map.lookup n (graphNodeToId g') of
             Just i  -> i
             Nothing -> error "edgesToVector: Graph node/ID mismatch."

vectorToEdges :: Graph d a -> Vector -> IO [Edge d a]
vectorToEdges g@(G _) v = do
  l <- vectorToList v
  return $ map (edgeIdToEdge g . round) l

vectorToVertices :: Graph d a -> Vector -> IO [a]
vectorToVertices g@(G _) v = fmap (map (idToNode'' g . round)) (vectorToList v)

vectorPtrToVertices :: Graph d a -> VectorP -> IO [[a]]
vectorPtrToVertices g@(G _) v = fmap (map (map (idToNode'' g . round))) (vectorPtrToList v)

vectorPtrToEdges :: Graph d a -> VectorP -> IO [[Edge d a]]
vectorPtrToEdges g@(G _) v = do
  l <- vectorPtrToList v
  return $ map (map (edgeIdToEdge g . round)) l


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

foreign import ccall "&c_igraph_destroy"                  c_igraph_destroy                    :: FunPtr (Ptr Void -> IO ())


--------------------------------------------------------------------------------
-- Helper Functions

forListM_ :: [a] -> (a -> IO b) -> IO ()
forListM_ []       _ = return ()
forListM_ (a : as) f = f a >> forListM_ as f

-- forListM :: [a] -> (a -> IO b) -> IO [b]
-- forListM = go []
--   where
--   go :: [b] -> [a] -> (a -> IO b) -> IO [b]
--   go acc [] _       = return (reverse acc)
--   go acc (a : as) f = f a >>= \b -> go (b : acc) as f
--
--
--
--
--------------------------------------------------------------------------------
-- Basics

getWeight :: Edge (Weighted d) a -> Int
getWeight (W _ w) = w

toEdgeWeighted :: E d a => a -> a -> Int -> Edge (Weighted d) a
toEdgeWeighted a b w = W (toEdge a b) w

emptyGraph :: E d a => Graph d a
emptyGraph = buildForeignGraph $ G (Graph 0 0 Map.empty Map.empty Set.empty undefined)

fromList :: E d a => [(a,a)] -> Graph d a
fromList = foldl' (\g (a,b) -> insertEdge (toEdge a b) g) emptyGraph

fromListWeighted :: (E d a, IsUnweighted d) => [(a,a,Int)] -> Graph (Weighted d) a
fromListWeighted = foldl' (\g (a,b,w) -> insertEdge (W (toEdge a b) w) g) emptyGraph

numberOfNodes :: Graph d a -> Int
numberOfNodes (G g) = graphNodeNumber g

numberOfEdges :: Graph d a -> Int
numberOfEdges (G g) = graphEdgeNumber g

member :: a -> Graph d a -> Bool
member a (G g) = a `Map.member` graphNodeToId g

nodeToId :: Graph d a -> a -> Maybe Int
nodeToId (G g) n = Map.lookup n (graphNodeToId g)

idToNode :: Graph d a -> Int -> Maybe a
idToNode (G g) i = Map.lookup i (graphIdToNode g)

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
deleteNode n (G g) = buildForeignGraph $ G $
  case Map.lookup n (graphNodeToId g) of
       Just i  -> g { graphNodeNumber = graphNodeNumber g - 1
                    , graphIdToNode   = Map.delete i (graphIdToNode g)
                    , graphNodeToId   = Map.delete n (graphNodeToId g)
                    , graphEdges      = Set.filter (\e -> edgeFrom e /= n && edgeTo e /= n) (graphEdges g)
                    }
       Nothing -> g

insertEdge :: Edge d a -> Graph d a -> Graph d a
insertEdge e (G g)
  | e `Set.member` edges (G g) || f == t = G g
  | otherwise = buildForeignGraph $ G $
    case (Map.member f (graphNodeToId g), Map.member t (graphNodeToId g)) of
         (True,  True)  -> insertEdge'' (G g)
         (False, True)  -> insertEdge'' (insertNode f i (G g))
         (True,  False) -> insertEdge'' (insertNode t i (G g))
         (False, False) -> insertEdge'' (insertNode t (i+1) $ insertNode f i (G g))
 where
  (f,t) = (edgeFrom e, edgeTo e)
  i     = Map.size (graphIdToNode g)

  insertEdge'' (G g') =
    g' { graphEdgeNumber = graphEdgeNumber g' + 1
       , graphEdges      = Set.insert e (graphEdges g')
       }

  insertNode :: a -> Int -> Graph d a -> Graph d a
  insertNode n ni (G g') = G $
    g' { graphNodeNumber = graphNodeNumber g' + 1
       , graphIdToNode   = Map.insert ni n  (graphIdToNode g')
       , graphNodeToId   = Map.insert n  ni (graphNodeToId g') }

deleteEdge :: Edge d a -> Graph d a -> Graph d a
deleteEdge e (G g)
  | Set.member e (graphEdges g) = buildForeignGraph $ deleteNodes $ G $
    g { graphEdges      = Set.delete e (graphEdges g)
      , graphEdgeNumber = graphEdgeNumber g - 1
      }
  | otherwise = G g
 where
  (f,t) = (edgeFrom e, edgeTo e)
  deleteNodes g' =
    let delF = if Set.null (neighbours f g') then deleteNode f else id
        delT = if Set.null (neighbours t g') then deleteNode t else id
     in delT . delF $ g'

nodes :: Graph d a -> HashSet a
nodes (G g) = Set.fromList $ Map.keys $ graphNodeToId g

edges :: Graph d a -> HashSet (Edge d a)
edges (G g) = graphEdges g

neighbours :: a -> Graph d a -> HashSet a
neighbours n g@(G _) =
  Set.foldr neighbours'' Set.empty (edges g)
 where
  neighbours'' e r
    | edgeFrom e == n                       = Set.insert (edgeTo   e) r
    | edgeTo   e == n && not (isDirected g) = Set.insert (edgeFrom e) r
    | otherwise                             = r
