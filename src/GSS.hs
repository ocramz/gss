{-# language GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- | Graph-structured stacks
module GSS (GSS,
            -- * Construction
            push,
            fork,
            combine,
            prune,
            -- empty,
            -- * Access
            gssAdjMap, gssTop,
            -- ** Internal types
            Label, labelId, NodeId, labelledNode,
            -- ** Build monad
            build,
            buildT,
            S,
            -- ** Validation
            isValid,
             -- * GraphViz support
            dotExport,
            dotWrite,
            ) where

import Control.Applicative (Alternative(..))
import Data.Bifunctor (second)
import Data.Function (on)
import Data.Int (Int32, Int64)
import Data.List (find)
import Data.Ord (comparing)

-- algebraic-graphs
import qualified Algebra.Graph.AdjacencyMap as G (AdjacencyMap, empty, vertex, edge, edges, overlay, connect, removeVertex, removeEdge, vertexSet, edgeSet, preSet, postSet, isEmpty)
import qualified Algebra.Graph.Labelled.AdjacencyMap as GL (AdjacencyMap, empty, vertex, edge, edges, overlay, connect, removeVertex, removeEdge, vertexSet, edgeSet, preSet, postSet, isEmpty)
import qualified Algebra.Graph.ToGraph as G (dfsForest)
import qualified Algebra.Graph.Export.Dot as G (exportViaShow)
-- containers
import qualified Data.Map as M (Map, fromList, toList, fromSet, mapKeys)
import qualified Data.Set as S (Set, empty, singleton, member, fromList, toList, map, insert, delete, filter, union, difference, partition, intersection, lookupMin, lookupMax)
import Data.Tree (Tree, rootLabel, Forest)
--
import Data.Text as T (Text)
import Data.Text.Lazy as TL (Text)
import Data.Text.Lazy.IO as TL (writeFile)
import Data.Text.Internal.Builder as TB (Builder, toLazyText)
-- transformers
import Control.Monad.Trans.State (StateT, State, runState, runStateT, execState, execStateT, get, put, modify)

-- | export GSS in graphviz Dot format
dotExport :: (Ord a, Show a, Eq e, Monoid e) => GSS e a -> TL.Text
dotExport = TB.toLazyText . G.exportViaShow . unGSS

-- | export GSS in graphviz Dot format and write it to file
dotWrite :: (Ord a, Show a, Eq e, Monoid e) =>
            FilePath -- ^ filename of output file
         -> GSS e a -- ^ GSS to be exported
         -> IO ()
dotWrite fp = TL.writeFile fp . dotExport

-- | Graph-structured stack
data GSS e a = GSS {
  unGSS :: GL.AdjacencyMap e (Label a)
  , tops :: S.Set (Label a)
                    } deriving (Eq, Show)

type NodeId = Int32
data Label a = Label NodeId a deriving (Eq, Ord)
instance Show a => Show (Label a) where
  show (Label i x) = "<" <> show x <> " " <> show i <> ">"
labelId :: Label a -> NodeId
labelId (Label i _) = i
labelledNode :: Label a -> a
labelledNode (Label _ x) = x

-- | The graph adjacency map underlying the 'GSS'
gssAdjMap :: GSS e a -> GL.AdjacencyMap e (Label a)
gssAdjMap = unGSS

-- | The top nodes of the 'GSS'
gssTop :: GSS e a -> S.Set (Label a)
gssTop = tops




-- | Push a node to the top of the GSS
--
-- NB: the edge label parameter corresponding to the first pushed node is ignored
push :: (Monad m, Monoid e, Ord a, Eq e) =>
        e -- ^ edge label
     -> a -- ^ node
     -> StateT (S e a) m ()
push e x = modifyGSS $ \am rs i ->
  let
    xl = Label i x
  in
    case S.lookupMin rs of
      Nothing ->
        (GL.vertex xl, S.singleton xl, succ i) -- empty GSS
      Just r -> -- GSS stack top has at least 1 node
        if length rs == 1 -- GSS is stack-shaped
        then
          let
            amNew = GL.edge e xl r
            am' = am `GL.overlay` amNew
            i' = i + 1
            rs' = removeInternalVertices am' (xl `S.insert` rs)
          in
            (am', rs', i')
        else
          if xl `S.member` rs -- node already in GSS tops
          then (am, rs, i) -- return original
          else
            let
              vs = S.toList rs
              n = length rs
              xls = labeledFrom i x n
              amNew = GL.edges (zip3 (repeat e) xls vs)
              am' = am `GL.overlay` amNew
              i' = i + fromIntegral (length rs)
              rs' = removeInternalVertices am' (xl `S.insert` rs)
            in (am', rs', i')


-- | Fork the stack into two or more replicas
fork :: (Monad m, Monoid e, Eq e, Ord a) =>
        Int -- ^ replicas of the stack
     -> e -- ^ edge label
     -> a -- ^ node
     -> a -- ^ top of the stack to which the forked stack will point
     -> StateT (S e a) m ()
fork n e x xtop
  | n > 1 = modifyGSS $ \am rs i ->
          case findLabeled xtop rs of
            Nothing -> (am, rs, i)
            Just xtopl ->
              let
                i' = i + 1
                xls = labeledFrom i' x n
                amNew = GL.edges (zip3 (repeat e) xls (repeat xtopl)) -- edges from all the labeled copies of x to xtop
                am' = am `GL.overlay` amNew
                i'' = i' + fromIntegral n
                rs' = removeInternalVertices am' (S.fromList xls `S.union` rs)
              in
                (am', rs', i'')
  | otherwise = pure ()


-- | Combine a set of stack tops by adding a common top that points to all of them
combine :: (Monad m, Monoid e, Eq e, Ord a) =>
           S.Set a -- ^ stack tops to be merged
        -> e -- ^ edge label
        -> a -- ^ new stack top
        -> StateT (S e a) m ()
combine ts e x = modifyGSS $ \am rs i ->
  if allTops ts rs
  then
    let
      n = length ts
      xl = labeledFromMulti i [x]
      i' = i + 1
      tls = labeledFromMulti i' (S.toList ts)
      amNew = GL.edges (zip3 (repeat e) xl tls)
      am' = am `GL.overlay` amNew
      i'' = i' + fromIntegral n
      rs' = removeInternalVertices am' (S.fromList xl `S.union` rs)
    in
      (am', rs', i'')
  else (am, rs, i)

allTops :: (Foldable t, Ord b) => S.Set b -> t (Label b) -> Bool
allTops ts = all ((`S.member` ts) . labelledNode)

-- | Prune a branch of a 'GSS' terminating in the given node
prune :: (Monad m, Ord a) =>
         a -- ^ top node of the branch to be pruned
      -> StateT (S e a) m ()
prune x = modifyGSS $ \am rs i ->
  case findLabeled x rs of
    Nothing -> (am, rs, i)
    Just xl ->
      let
        am' = removeChain am xl
        rs' = S.delete xl rs
      in
        (am', rs', i)


findLabeled :: Eq a =>
               a -> S.Set (Label a) -> Maybe (Label a)
findLabeled x = find ((== x) . labelledNode)

removeChain :: (Ord a) => GL.AdjacencyMap e a -> a -> GL.AdjacencyMap e a
removeChain = go
  where
    go am x = case (S.lookupMin ps, length ps) of
      (Nothing, _) -> am
      (Just x', 1) -> if length (GL.preSet x' am) == 1
                      then go am' x'
                      else am
      _ -> error "invariant broken : post-set should be a singleton"
      where
        am' = GL.removeVertex x am
        ps = GL.postSet x am

labeledFrom :: NodeId -- ^ starting node label
            -> a
            -> Int -- ^ number of copies
            -> [Label a]
labeledFrom i0 x n = labeledFromMulti i0 (replicate n x)

labeledFromMulti :: NodeId
                 -> [b] -- ^ NB : must be finite length
                 -> [Label b]
labeledFromMulti i0 = zipWith Label [i0, i0+1 ..]

isValid :: Ord a => GSS e a -> Bool
isValid (GSS am rs) = all (\r -> null (GL.preSet r am)) rs


isChainLink :: Ord a => a -> GL.AdjacencyMap e a -> Bool
isChainLink x am =
  let
    ps = GL.postSet x am
  in
    case getSetSingleton ps of
      Nothing -> True
      Just xnext ->
        let
          pres = GL.preSet xnext am
        in
          case getSetSingleton pres of
            Just xpre -> xpre == x
            _ -> False


getSetSingleton :: Eq a => S.Set a -> Maybe a
getSetSingleton s = case (S.lookupMin s, S.lookupMax s) of
      (Just xmi, Just xma) | xmi == xma -> Just xmi
      _ -> Nothing


-- isChainGraph x (GSS am rs) = go True where case findLabeled x rs of
--   Just xl -> if

-- | if the pre-set of any point in the set is non-empty we remove it from the set
--
-- this is to maintain the invariant that GSS stack tops (the 'tops' set) do not have incoming edges
removeInternalVertices :: Ord a =>
                          GL.AdjacencyMap e a
                       -> S.Set a -- ^ candidate stack tops
                       -> S.Set a
removeInternalVertices am rs = foldr remf rs rs
  where
    remf r acc =
      let ps = GL.preSet r am
      in if not (null ps)
         then S.delete r acc
         else acc




-- modifyGSS :: Monad m =>
--              (GL.AdjacencyMap e a -> S.Set a -> NodeId -> (GL.AdjacencyMap e a, S.Set a, NodeId))
--           -> StateT (S e a) m ()
modifyGSS :: Monad m =>
             (GL.AdjacencyMap e (Label a) -> S.Set (Label a) -> NodeId -> (GL.AdjacencyMap e (Label a), S.Set (Label a), NodeId))
          -> StateT (S e a) m ()
modifyGSS f = modify (modifySGss f)

data S e a = S {
  sGss :: GSS e a
  , sNodeCounter :: !NodeId -- ^ used to disambiguate duplicate nodes pushed on candidate branches of the GSS
               } deriving (Eq, Show)
s0 :: S e a
s0 = S gssEmpty 0

-- modifySGss :: (GL.AdjacencyMap e1 a1 -> S.Set a1 -> NodeId -> (GL.AdjacencyMap e2 a2, S.Set a2, NodeId))
--            -> S e1 a1 -> S e2 a2
modifySGss :: (GL.AdjacencyMap e1 (Label a1) -> S.Set (Label a1) -> NodeId -> (GL.AdjacencyMap e2 (Label a2), S.Set (Label a2), NodeId))
           -> S e1 a1 -> S e2 a2
modifySGss f (S (GSS am rs) i) = let (am', rs', i') = f am rs i in S (GSS am' rs') i'

-- | Build the 'GSS'
build :: State (S e a) b -> (GSS e a)
build m = sGss $ execState m s0

-- | Build the 'GSS'
buildT :: (Monad m) => StateT (S e a) m b -> m (GSS e a)
buildT m = sGss <$> execStateT m s0

-- | Empty 'GSS'
gssEmpty :: GSS e a
gssEmpty = GSS GL.empty S.empty







-- p0 :: GSS [Char] Char
-- p0 = build (push "a" '.' >> push "b" '.' >> push "c" '.' >> fork 2 "f" '.' '.')

-- -- old

-- removeVertices :: (Foldable t, Ord a) => G.AdjacencyMap a -> t a -> G.AdjacencyMap a
-- removeVertices = foldr G.removeVertex

-- | Pop the top nodes from the top of the GSS

-- pop :: (Monad m, Ord a) =>
--        (a -> Bool) -- ^ predicate for selecting top nodes
--     -> StateT (GSS a) m (S.Set a)
-- pop f = do
--   GSS am vs <- get
--   let
--     (vsOk, vsNo) = S.partition f vs
--     am' = removeVertices am vsOk
--     gss' = GSS am' vsNo
--   put gss'
--   pure vsOk

-- push :: (Monad m, Ord a) =>
--         (a -> a -> Bool) -- ^ node comparison
--      -> a -- ^ node to push
--      -> StateT (GSS a) m ()
-- push f x = modifyGSS $ \am rs ->
--                          if x `S.member` rs
--                          then (am, rs)
--                          else
--                            let
--                              vs = S.toList $ S.filter (f x) rs
--                              amNew = G.edges $ zip (repeat x) vs
--                              am' = am `G.overlay` amNew
--                            in (am', removeInternalVertices am' ( x `S.insert` rs) )



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
