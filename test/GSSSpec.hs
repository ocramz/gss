{-# OPTIONS_GHC -Wno-unused-imports #-}
module GSSSpec (spec) where

-- containers
import qualified Data.Set as S (Set, toList, fromList)
-- hspec
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
-- transformers
import Control.Monad.Trans.State (StateT, State)

import GSS (GSS, push, pop, build, gssRoots, gssAdjMap)


spec :: Spec
spec =
  describe "GSS construction" $ do
    it "push (>) should build a chain graph" $ do
      let
        g0 = snd $ build (pushGt 'a' >> pushGt 'b' >> pushGt 'c')
        rs = gssRoots g0
      rs `shouldBe` S.fromList ['c']


-- g0 :: GSS Char
-- g0 = snd $ build $ do
--   pushGt 'a'
--   pushGt 'b'
--   pushGt 'c'

pushGt :: Ord a => a -> State (GSS a) ()
pushGt = push (>)
