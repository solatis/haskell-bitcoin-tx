module Data.Bitcoin.Transaction where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base16.Lazy as BS16L

import qualified Data.Binary     as B ( decode
                                      , encode )

import Data.Bitcoin.Transaction.Types

-- | Decodes a hex representation of a transaction into a 'Transaction' object.
decode :: BSL.ByteString -> Transaction
decode =
  B.decode . fst . BS16L.decode

-- | Encodes a 'Transaction' object into a hex representation.
encode :: Transaction -> BSL.ByteString
encode =
  BS16L.encode . B.encode
