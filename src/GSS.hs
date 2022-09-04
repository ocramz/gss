{-# language GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- | Graph-structured stacks
module GSS (GSS,
            -- * Construction
            push,
            pop,
            -- ** Helpers
             build,
             -- * GraphViz support
            dotExport,
            dotWrite,
           )where

-- import Data.Bifunctor (second)
import Data.Function (on)
import Data.Ord (comparing)

-- algebraic-graphs
import qualified Algebra.Graph.AdjacencyMap as G (AdjacencyMap, vertex, edge, edges, overlay, connect, removeVertex, removeEdge, vertexSet, edgeSet)
import qualified Algebra.Graph.ToGraph as G (dfsForest)
import qualified Algebra.Graph.Export.Dot as G (exportViaShow)
-- containers
import qualified Data.Set as S (Set, fromList, toList, insert, filter, partition)
import Data.Tree (Tree, rootLabel, Forest)
--
import Data.Text as T (Text)
import Data.Text.Lazy as TL (Text)
import Data.Text.Lazy.IO as TL (writeFile)
import Data.Text.Internal.Builder as TB (Builder, toLazyText)
-- transformers
import Control.Monad.Trans.State (StateT, State, runState, execState, get, put, modify)

-- | export GSS in graphviz Dot format
dotExport :: (Ord a, Show a) => GSS a -> TL.Text
dotExport = TB.toLazyText . G.exportViaShow . unGSS

-- | export GSS in graphviz Dot format and write it to file
dotWrite :: (Ord a, Show a) =>
            FilePath -- ^ filename of output file
         -> GSS a -> IO ()
dotWrite fp = TL.writeFile fp . dotExport

-- | Graph-structured stack
newtype GSS a = GSS {
  unGSS :: G.AdjacencyMap a
                    } deriving (Eq, Show, Semigroup, Monoid)

build :: Ord a => State (GSS a) o -> G.AdjacencyMap a
build m = unGSS $ execState m mempty

-- | Pop a node from the top of the GSS
pop :: (Ord a) =>
       (a -> a -> Bool) -- ^ node comparison function
    -> a -- ^ node to be popped
    -> State (GSS a) ()
pop f x = modifyGSS $ \am ->
                        let
                          -- vs = S.toList $ S.filter (f x) (G.vertexSet am)
                          vs = filter (f x) $ dfsRoots am
                          es = zip vs $ repeat x
                        in
                          G.removeVertex x $ removeEdges am es

removeEdges :: (Foldable t, Ord a) =>
               G.AdjacencyMap a
            -> t (a, a) -> G.AdjacencyMap a
removeEdges = foldr (\(v1, v2) am -> G.removeEdge v1 v2 am)

dfsRoots :: (Ord a) => G.AdjacencyMap a -> [a]
dfsRoots = map rootLabel . G.dfsForest


-- | Push a node to the top of the GSS
push :: (Ord a) =>
        (a -> a -> Bool) -- ^ node comparison function
     -> a -- ^ node to be pushed
     -> State (GSS a) ()
push f x = modifyGSS $ \am ->
                             let
                               -- vs = S.toList $ S.filter (f x) (G.vertexSet am)
                               vs = filter (f x) $ dfsRoots am
                               amNew = G.edges (zip (repeat x) vs) -- edges from x to all vertices in vs
                             in
                               am `G.overlay` amNew

modifyGSS :: Monad m =>
             (G.AdjacencyMap a -> G.AdjacencyMap a)
          -> StateT (GSS a) m ()
modifyGSS f = modify (withGSS f)

withGSS :: (G.AdjacencyMap a -> G.AdjacencyMap b)
        -> GSS a -> GSS b
withGSS f (GSS x) = GSS (f x)
