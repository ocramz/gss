{-# OPTIONS_GHC -Wno-unused-imports #-}
module GSSSpec (spec) where

-- containers
import qualified Data.Set as S (Set, toList, fromList, map)
-- hspec
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
-- transformers
import Control.Monad.Trans.State (StateT, State)

import GSS (GSS, S, push, fork, build, gssTop, gssAdjMap, labelledNode)


spec :: Spec
spec =
  describe "GSS construction" $ do
    it "push >> push >> push should build a chain graph" $ do
      let
        g0 = build (push' 'a' >> push' 'b' >> push' 'c')
        rs = S.map labelledNode $ gssTop g0
      rs `shouldBe` S.fromList ['c']
    it "push >> fork should build a Y-shaped fork" $ do
      let
        g1 = build (push' 'a' >> push' 'b' >> fork' 'z' 'b')
        rs = S.map labelledNode $ gssTop g1
      rs `shouldBe` S.fromList ['z', 'z']


push' :: Ord a => a -> State (S [Char] a) ()
push' = push "_"

fork' :: (Monad m, Ord a) => a -> a -> StateT (S String a) m ()
fork' = fork 2 "_"
