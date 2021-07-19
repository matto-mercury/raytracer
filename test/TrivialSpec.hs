module TrivialSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "hspec" $ do
    it "works" $
      True `shouldBe` True