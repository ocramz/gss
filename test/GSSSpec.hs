module GSSSpec where


import qualified Data.Set as S (Set, toList, fromList)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import GSS (GSS, push, pop, build, gssRoots, gssAdjMap)


spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True

