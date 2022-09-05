{-# language GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- | Graph-structured stacks
module GSS (GSS,
            -- * Access
            gssAdjMap, gssRoots,
            -- * Construction
            push,
            pop,
            -- empty,
            build,
             -- * GraphViz support
            dotExport,
            dotWrite,
           )where

-- import Data.Bifunctor (second)
import Data.Function (on)
import Data.Ord (comparing)

-- algebraic-graphs
import qualified Algebra.Graph.AdjacencyMap as G (AdjacencyMap, vertex, edge, edges, overlay, connect, removeVertex, removeEdge, vertexSet, edgeSet, preSet)
import qualified Algebra.Graph.ToGraph as G (dfsForest)
import qualified Algebra.Graph.Export.Dot as G (exportViaShow)
-- containers
import qualified Data.Set as S (Set, member, fromList, toList, insert, delete, filter, difference, partition, intersection)
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
         -> GSS a -- ^ GSS to be exported
         -> IO ()
dotWrite fp = TL.writeFile fp . dotExport

-- | Graph-structured stack
data GSS a = GSS {
  unGSS :: G.AdjacencyMap a
  , roots :: S.Set a
                    } deriving (Eq, Show)



-- instance (Ord a) => Semigroup (GSS a) where
--   GSS a ra <> GSS b rb = GSS (a <> b) (ra <> rb)
-- instance (Ord a) => Monoid (GSS a) where
--   mempty = GSS mempty mempty

-- | The graph adjacency map underlying the 'GSS'
gssAdjMap :: GSS a -> G.AdjacencyMap a
gssAdjMap = unGSS

-- | The top nodes of the 'GSS'
gssRoots :: GSS a -> S.Set a
gssRoots = roots

-- | Push a node to the top of the GSS
push :: Ord a =>
        (a -> a -> Bool) -- ^ node comparison
     -> a -- ^ node to push
     -> State (GSS a) ()
push f x = modifyGSS $ \am rs ->
                         if x `S.member` rs
                         then (am, rs)
                         else
                           let
                             vs = S.toList $ S.filter (f x) rs
                             amNew = G.edges $ zip (repeat x) vs
                             am' = am `G.overlay` amNew
                           in (am', removeInternalVertices am' ( x `S.insert` rs) )

-- | Pop the top nodes from the top of the GSS
pop :: (Ord a) =>
       (a -> Bool) -- ^ predicate for selecting top nodes
    -> State (GSS a) (S.Set a)
pop f = do
  GSS am vs <- get
  let
    (vsOk, vsNo) = S.partition f vs
    am' = removeVertices am vsOk
    gss' = GSS am' vsNo
  put gss'
  pure vsOk

modifyGSS :: (G.AdjacencyMap a -> S.Set a -> (G.AdjacencyMap a, S.Set a))
          -> State (GSS a) ()
modifyGSS f = modify $ \(GSS am rs) ->
                         let (am', rs') = f am rs
                         in GSS am' rs'


-- | Build the 'GSS'
build :: Ord a => State (GSS a) b -> (b, GSS a)
build m = runState m empty

-- | Empty 'GSS'
empty :: (Ord a) => GSS a
empty = GSS mempty mempty


-- | if the pre-set of any point in the set is non-empty we remove it from the set
removeInternalVertices :: Ord a =>
                          G.AdjacencyMap a
                       -> S.Set a -- ^ candidate root points
                       -> S.Set a
removeInternalVertices am rs = foldr remf rs rs
  where
    remf r acc =
      let ps = G.preSet r am
      in if not (null ps)
         then S.delete r acc
         else acc

removeVertices :: (Foldable t, Ord a) => G.AdjacencyMap a -> t a -> G.AdjacencyMap a
removeVertices = foldr G.removeVertex




-- -- old

-- -- | Pop a node from the top of the GSS
-- pop :: (Ord a) =>
--        (a -> a -> Bool) -- ^ node comparison function
--     -> a -- ^ node to be popped
--     -> State (GSS a) ()
-- pop f x = modifyGSS $ \am ->
--                         let
--                           -- vs = S.toList $ S.filter (f x) (G.vertexSet am)
--                           vs = filter (f x) $ dfsRoots am
--                           es = zip vs $ repeat x
--                         in
--                           G.removeVertex x $ removeEdges am es

-- removeEdges :: (Foldable t, Ord a) =>
--                G.AdjacencyMap a
--             -> t (a, a) -> G.AdjacencyMap a
-- removeEdges = foldr (\(v1, v2) am -> G.removeEdge v1 v2 am)

-- dfsRoots :: (Ord a) => G.AdjacencyMap a -> [a]
-- dfsRoots = map rootLabel . G.dfsForest


-- -- | Push a node to the top of the GSS
-- push :: (Ord a) =>
--         (a -> a -> Bool) -- ^ node comparison function
--      -> a -- ^ node to be pushed
--      -> State (GSS a) ()
-- push f x = modifyGSS $ \am ->
--                              let
--                                -- vs = S.toList $ S.filter (f x) (G.vertexSet am)
--                                vs = filter (f x) $ dfsRoots am
--                                amNew = G.edges (zip (repeat x) vs) -- edges from x to all vertices in vs
--                              in
--                                am `G.overlay` amNew



-- -- modifyGSS :: Monad m =>
-- --              (G.AdjacencyMap a -> G.AdjacencyMap a)
-- --           -> StateT (GSS a) m ()
-- modifyGSS f = modify (withGSS f)

-- withGSS :: (G.AdjacencyMap a -> G.AdjacencyMap b)
--         -> GSS a -> GSS b
-- withGSS f (GSS am rs) = GSS (f am rs)
