module Bitcoin.TransactionSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "when building a new transaction" $ do
    it "should work properly" $
      True `shouldBe` True
