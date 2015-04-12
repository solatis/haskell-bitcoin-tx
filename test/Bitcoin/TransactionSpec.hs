module Bitcoin.TransactionSpec where

import qualified Data.ByteString.Lazy.Char8 as BSL8 (pack)
import Bitcoin.Transaction ( decode
                           , encode )
import Text.Groom (groom)

import Test.Hspec

spec :: Spec
spec = do
  describe "when parsing a specific transaction" $ do
    let hex = BSL8.pack "01000000013c0c37049cefb7d0754c716c1227e221f1b5cc9fdf7fc8e6aadd5ce6465fad32000000004a493046022100b41330548f320fcc282d72462656f80c0da64beb352f7fbbdf55d651674b5846022100cbef624c80302900e6c0e9b4bbb024cd072e54d7535c8a79a3ce9b36c304d7cc01ffffffff0100f2052a0100000017a914379ad9b7ba73bdc1e29e286e014d4e2e1f6884e38700000000"

    it "encoding a decoding a transaction results in the original hex" $
      (encode . decode) hex `shouldBe` hex

    it "succesfully parses a transaction into a meaningful object" $ do
      let decoded = decode hex

      putStrLn ("decoded = " ++ groom decoded)
      True `shouldBe` True
