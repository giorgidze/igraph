{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}

-- | Module for basic `Graph' functions without access to C libraries
module Data.IGraph.Basics where

import qualified Data.HashMap.Strict as Map
import qualified Data.Map as M

import Data.HashSet (HashSet)
import qualified Data.HashSet as Set

import Data.List
import Control.Monad.State
import Foreign
import System.IO

import Data.IGraph.Types
