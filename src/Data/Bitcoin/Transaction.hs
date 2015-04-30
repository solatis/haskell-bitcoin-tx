module Data.Bitcoin.Transaction ( decode
                                , encode
                                , transactionId
                                , Transaction (..)
                                , TransactionIn (..)
                                , TransactionOut (..)
                                , Coinbase ) where

import qualified Data.Binary                    as B (encode)

import qualified Data.ByteString                as BS (reverse)
import qualified Data.ByteString.Lazy           as BSL (toStrict)

import qualified Crypto.Hash.SHA256             as Sha256
import qualified Data.HexString                 as HS

import Data.Bitcoin.Types ( TransactionId )

import           Data.Bitcoin.Transaction.Types

-- | Decodes a hex representation of a transaction into a 'Transaction' object.
decode :: HS.HexString -- ^ The hexadecimal representation of the transaction
       -> Transaction  -- ^ The decoded 'Transaction' object
decode = HS.toBinary

-- | Encodes a 'Transaction' object into a hex representation.
encode :: Transaction  -- ^ The 'Transaction' we would like to encode to hex
       -> HS.HexString -- ^ The hexadecimal representation of the transaction
encode = HS.fromBinary

-- | Calculates the transaction id of a 'Transaction' as a 'TransactionId' so it
--   can be used in RPC interfaces.
transactionId :: Transaction -> TransactionId
transactionId =
      -- Bitcoin uses a "double sha256", also known as sha256d as its hash algo
  let sha256d = Sha256.hash . Sha256.hash
      bytes   = BSL.toStrict . B.encode

  in HS.fromBytes . BS.reverse . sha256d . bytes
