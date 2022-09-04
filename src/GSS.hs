{-# language GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module GSS where

-- algebraic-graphs
import qualified Algebra.Graph.AdjacencyMap as G (AdjacencyMap, vertex, edge, connect, removeVertex, removeEdge)
-- transformers
import Control.Monad.Trans.State (State, runState, execState, get, put, modify)


newtype GSS a = GSS {
  unGSS :: G.AdjacencyMap a
                    } deriving (Eq, Show, Semigroup, Monoid)

-- pushWith f (GSS g)
