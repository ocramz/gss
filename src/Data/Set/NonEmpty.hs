{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Data.Set.NonEmpty (
  Set,
  -- * Construction
  singleton, fromList, nonEmpty
  -- * Insertion
  , insert
  -- * Access
  , head
  -- * Deletion
  , delete
  -- * Folds
  -- , size
  -- ** Helpers
  , valid
                         ) where

import qualified Data.Foldable as F (fold, toList, elem)
import Data.List.NonEmpty (NonEmpty(..))

-- containers
import qualified Data.Set as S (Set, empty, size, singleton, member, fromList, toList, insert, foldl, foldl', delete, filter, difference, partition, intersection, valid, minView)

import Prelude hiding (head, foldl', foldl1)

-- | Nonempty sets
data Set a = Set a (S.Set a) deriving (Eq, Show, Ord)
instance Foldable Set where
    fold      (Set x s) = x <> F.fold s
    {-# INLINE fold #-}
    foldMap f (Set x s) = f x <> foldMap f s
    {-# INLINE foldMap #-}
    length = size
    {-# INLINE length #-}
    null _ = False
    {-# INLINE null #-}
    elem x (Set x0 s) = F.elem x s
                        || x == x0
    {-# INLINE elem #-}
    -- foldl'  = foldl'
    -- {-# INLINE foldl' #-}
    -- foldl1  = foldl1
    -- {-# INLINE foldl1 #-}

size :: Set a -> Int
size (Set _ s) = 1 + S.size s
{-# INLINE size #-}


fromList :: Ord a => NonEmpty a -> Set a
fromList (x :| xs) = Set x (S.fromList xs)

nonEmpty :: Ord a => [a] -> Maybe (Set a)
nonEmpty [] = Nothing
nonEmpty (x:xs) = Just (Set x (S.fromList xs))

-- | Internal validity
valid :: Ord a => Set a -> Bool
valid (Set x xs) = not (x `S.member` xs)

-- | singleton set
singleton :: a -> Set a
singleton x = Set x S.empty

-- | insert an element
insert :: Ord a => a -> Set a -> Set a
insert x s@(Set y ys)
  | x == y = s
  | otherwise = Set x (S.insert y ys)

-- | head element of a set
head :: Set a -> a
head (Set x _ ) = x

-- | delete an element from the set
delete :: Ord a => a -> Set a -> Maybe (Set a)
delete y s@(Set x xs)
  | null xs && y == x = Nothing
  | null xs = Just s
  | otherwise = Just (Set x (S.delete y xs))


-- delete :: Ord a => a -> NESet a -> Set a
-- delete x n@(Set x0 s) = case compare x x0 of
--     LT -> toSet n
--     EQ -> s
--     GT -> insertMinSet x0 . S.delete x $ s
-- {-# INLINE delete #-}



-- foldl1 :: (a -> a -> a) -> Set a -> a
-- foldl1 f (Set x s) = S.foldl f x s
-- {-# INLINE foldl1 #-}

-- foldl' :: (a -> b -> a) -> a -> Set b -> a
-- foldl' f z (Set x s) = S.foldl' f y s
--   where
--     !y = f z x
-- {-# INLINE foldl' #-}
