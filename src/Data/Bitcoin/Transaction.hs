module Data.Bitcoin.Transaction ( decode
                                , encode
                                , Transaction (..)
                                , TransactionIn (..)
                                , TransactionOut (..)) where

import qualified Data.HexString as HS

import Data.Bitcoin.Transaction.Types

-- | Decodes a hex representation of a transaction into a 'Transaction' object.
decode :: HS.HexString -> Transaction
decode = HS.fromHex

-- | Encodes a 'Transaction' object into a hex representation.
encode :: Transaction -> HS.HexString
encode = HS.toHex

calculateTransactionId :: Transaction -> HS.HexString
calculateTransactionId tx = undefined
