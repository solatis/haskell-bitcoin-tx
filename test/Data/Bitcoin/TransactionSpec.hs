module Data.Bitcoin.TransactionSpec where

import           Data.Bitcoin.Script            (Script (..))
import           Data.Bitcoin.Transaction
import qualified Data.ByteString.Char8          as BS8 (pack)

import qualified Data.HexString as HS (hexString)

import           Test.Hspec

spec :: Spec
spec = do
  describe "when parsing a specific transaction" $ do
    let hex = HS.hexString $ BS8.pack "0100000002f327e86da3e66bd20e1129b1fb36d07056f0b9a117199e759396526b8f3a20780000000049483045022100fce442ec52aa2792efc27fd3ad0eaf7fa69f097fdcefab017ea56d1799b10b2102207a6ae3eb61e11ffaba0453f173d1792f1b7bb8e7422ea945101d68535c4b474801fffffffff0ede03d75050f20801d50358829ae02c058e8677d2cc74df51f738285013c26000000006b483045022100b77f935ff366a6f3c2fdeb83589c790265d43b3d2cf5e5f0047da56c36de75f40220707ceda75d8dcf2ccaebc506f7293c3dcb910554560763d7659fb202f8ec324b012102240d7d3c7aad57b68aa0178f4c56f997d1bfab2ded3c2f9427686017c603a6d6ffffffff02f028d6dc010000001976a914ffb035781c3c69e076d48b60c3d38592e7ce06a788ac00ca9a3b000000001976a914fa5139067622fd7e1e722a05c17c2bb7d5fd6df088ac00000000"

    it "encoding a decoding a transaction results in the original hex" $
      (encode . decode) hex `shouldBe` hex

    it "succesfully parses a transaction into a meaningful object" $ do
      let decoded = decode hex

      case decoded of
       (Transaction 1 [(TransactionIn _ _ 4294967295), (TransactionIn _ _ 4294967295)] [(TransactionOut 7999990000 (Script _)), (TransactionOut 1000000000 (Script _))] 0) -> return ()
       _                               -> expectationFailure ("Result does not match expected: " ++ show decoded)
