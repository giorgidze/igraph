{-# LANGUAGE PatternGuards, ForeignFunctionInterface #-}

module Data.IGraph
  ( Graph (..), G, Gr (..), D, U
    -- * Construction
  , emptyGraph, fromList
  , insertEdge, deleteEdge, deleteNode
    -- * Query
  , numberOfNodes, numberOfEdges, member, nodes, edges, neighbours

    -- * Chapter 13\. Structural Properties of Graphs
    -- ** 13\.1 Basic properties
  , areConnected
    -- ** 13\.2 Shortest Path Related Functions
  , getShortestPath, NeiMode(..)

    -- ** 13\.4 Graph Components
  , isConnected, Connectedness(..)

    -- * stupid, remove again
  , vectorToList, listToVector, edgesToVector, vectorToEdges
  ) where

import Data.IGraph.Basics
import Data.IGraph.Internal
import Data.IGraph.Internal.Constants
import Data.IGraph.Types

import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable
import System.IO.Unsafe

--------------------------------------------------------------------------------
-- 1. Basic properties

foreign import ccall "igraph_are_connected"
  c_igraph_are_connected :: GraphPtr -> CInt -> CInt -> Ptr CInt -> IO CInt

areConnected :: Graph d a -> a -> a -> Bool
areConnected g@(G _) n1 n2
  | Just i1 <- nodeToId g n1
  , Just i2 <- nodeToId g n2
  = unsafePerformIO $ withGraph_ g $ \gp -> alloca $ \bp -> do
    e <- c_igraph_are_connected gp (fromIntegral i1) (fromIntegral i2) bp
    if e == 0 then
       peek bp >>= return . (== 1)
     else
       error $ "areConnected: igraph error " ++ show e
  | otherwise = False

--------------------------------------------------------------------------------
-- 2. Shortest Path Related Functions

--shortestPath :: Graph d a -> a -> a -> ?

foreign import ccall "igraph_get_shortest_path"
  c_igraph_get_shortest_path :: GraphPtr -> VectorPtr -> VectorPtr -> CInt -> CInt -> CInt -> IO CInt

getShortestPath :: Graph d a -> a -> a -> NeiMode -> ([a],[Edge d a])
getShortestPath g@(G _) n1 n2 m
  | Just i1 <- nodeToId g n1
  , Just i2 <- nodeToId g n2
  = unsafePerformIO $ withGraph_ g $ \gp -> do
    v1 <- listToVector ([] :: [Int])
    v2 <- listToVector ([] :: [Int])
    e <- withVector v1 $ \vp1 -> withVector v2 $ \vp2 ->
         c_igraph_get_shortest_path gp vp1 vp2 (fromIntegral i1) (fromIntegral i2) (fromIntegral (fromEnum m))
    if e == 0 then do
       vert <- vectorToVertices g v1
       edgs <- vectorToEdges    g v2
       return ( vert, edgs )
     else
       error $ "getShortestPath: igraph error " ++ show e
  | otherwise = error "getShortestPath: Invalid nodes"

--------------------------------------------------------------------------------
-- 4. Graph Components

foreign import ccall "igraph_is_connected"
  c_igraph_is_connected :: GraphPtr -> Ptr CInt -> CInt -> IO CInt

isConnected :: Graph d a -> Connectedness -> Bool
isConnected g@(G _) c = unsafePerformIO $ withGraph_ g $ \gp -> alloca $ \b -> do
  _ <- c_igraph_is_connected gp b (fromIntegral $ fromEnum c)
  r <- peek b
  return $ r == 1
