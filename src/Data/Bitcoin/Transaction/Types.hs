{-# LANGUAGE FlexibleInstances  #-}

module Data.Bitcoin.Transaction.Types where

import Control.Applicative ( (<$>)
                           , (<*>) )
import Control.Monad ( liftM2
                     , replicateM
                     , forM_
                     , unless )

import Data.Word ( Word32
                 , Word64 )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Data.Bits (shiftL, shiftR)
import Data.Binary ( Binary, get, put, encode, decode )

import Data.Binary.Get ( getByteString
                       , getWord32le
                       , getWord64le
                       , getWord64be )

import Data.Binary.Put ( putByteString
                       , putWord32le
                       , putWord64le
                       , putWord64be )

import Data.Bitcoin.Types ( VarInt (..) )
import qualified Data.Bitcoin.Script as Btc ( Script (..) )

data TxnOutputType = TxnPubKey     -- ^ JSON of "pubkey" received.
                   | TxnPubKeyHash -- ^ JSON of "pubkeyhash" received.
                   | TxnScriptHash -- ^ JSON of "scripthash" received.
                   | TxnMultisig   -- ^ JSON of "multisig" received.
    deriving ( Show, Read, Ord, Eq )

data TransactionHash = TransactionHash Integer
    deriving ( Show, Read, Eq )

instance Binary TransactionHash where
    get = do
      a <- fromIntegral <$> getWord64be
      b <- fromIntegral <$> getWord64be
      c <- fromIntegral <$> getWord64be
      d <- fromIntegral <$> getWord64be

      return $ TransactionHash ((a `shiftL` 192) + (b `shiftL` 128) + (c `shiftL` 64) + d)

    put (TransactionHash i) = do
      putWord64be $ fromIntegral (i `shiftR` 192)
      putWord64be $ fromIntegral (i `shiftR` 128)
      putWord64be $ fromIntegral (i `shiftR` 64)
      putWord64be $ fromIntegral i

-- | The OutPoint is used inside a transaction input to reference the previous
-- transaction output that it is spending.
data OutPoint = OutPoint {
  -- | The hash of the referenced transaction.
  outPointHash  :: TransactionHash,

  -- | The position of the specific output in the transaction.
  -- The first output position is 0.
  outPointIndex :: Word32
  } deriving (Read, Show, Eq)

instance Binary OutPoint where
  get = do
    (h,i) <- liftM2 (,) get getWord32le
    return $ OutPoint h i

  put (OutPoint h i) = put h >> putWord32le i

-- | Data type representing a transaction input.
data TransactionIn =  TransactionIn {
  -- | Reference the previous transaction output (hash + position)
  prevOutput   :: OutPoint,

  -- | Script providing the requirements of the previous transaction
  -- output to spend those coins.
  scriptInput  :: Btc.Script,

  -- | Transaction version as defined by the sender of the
  -- transaction. The intended use is for replacing transactions with
  -- new information before the transaction is included in a block.
  txInSequence :: Word32

  } deriving (Eq, Show, Read)

instance Binary TransactionIn where
    get = do
      o <- get
      (VarInt len) <- get
      scriptBs <- getByteString (fromIntegral len)
      s <- getWord32le

      let i = decode $ BSL.fromStrict scriptBs

      return $ TransactionIn o i s

    put (TransactionIn o i s) = do
      let scriptBs = BSL.toStrict $ encode i

      put o
      put $ VarInt $ fromIntegral $ BS.length scriptBs
      putByteString scriptBs
      putWord32le s


-- | Data type representing a transaction output.
data TransactionOut = TransactionOut {
  -- | Transaction output value.
  outValue     :: Word64,

  -- | Script specifying the conditions to spend this output.
  scriptOutput :: Btc.Script

  } deriving (Eq, Show, Read)

instance Binary TransactionOut where
    get = do
        val <- getWord64le
        (VarInt len) <- get

        scriptBs <- getByteString (fromIntegral len)
        let s = decode $ BSL.fromStrict scriptBs

        return $ TransactionOut val s

    put (TransactionOut o s) = do
      let scriptBs = BSL.toStrict $ encode s

      putWord64le o
      put $ VarInt $ fromIntegral $ BS.length scriptBs
      putByteString scriptBs

-- | Data type representing a bitcoin transaction
data Transaction = Transaction {
  -- | Transaction data format version
  txVersion  :: Word32,

  -- | List of transaction inputs
  txIn       :: [TransactionIn],

  -- | List of transaction outputs
  txOut      :: [TransactionOut],

  -- | The block number of timestamp at which this transaction is locked
  txLockTime :: Word32

  } deriving (Eq, Show, Read)

instance Binary Transaction where
    get = Transaction <$> getWord32le
                      <*> (replicateList =<< get)
                      <*> (replicateList =<< get)
                      <*> getWord32le
      where
        replicateList (VarInt c) = replicateM (fromIntegral c) get

    put (Transaction v is os l) = do
        putWord32le v
        put $ VarInt $ fromIntegral $ length is
        forM_ is put
        put $ VarInt $ fromIntegral $ length os
        forM_ os put
        putWord32le l


-- | Data type representing the coinbase transaction of a 'Block'. Coinbase
--   transactions are special types of transactions which are created by miners
--   when they find a new block. Coinbase transactions have no inputs. They have
--   outputs sending the newly generated bitcoins together with all the block's
--   fees to a bitcoin address (usually the miners address). Data can be embedded
--   in a Coinbase transaction which can be chosen by the miner of a block. This
--   data also typically contains some randomness which is used, together with
--   the nonce, to find a partial hash collision on the block's hash.
data Coinbase = Coinbase {

  -- | Transaction data format version.
  cbVersion    :: Word32,

  -- | Previous outpoint. This is ignored for
  --   coinbase transactions but preserved for computing
  --   the correct txid.
  cbPrevOutput :: OutPoint,

  -- | Data embedded inside the coinbase transaction.
  cbData       :: BS.ByteString,

  -- | Transaction sequence number. This is ignored for
  --   coinbase transactions but preserved for computing
  --   the correct txid.
  cbInSequence :: Word32,

  -- | List of transaction outputs.
  cbOut        :: [TransactionOut],

  -- | The block number of timestamp at which this
  --   transaction is locked.
  cbLockTime   :: Word32

  } deriving (Eq, Show, Read)

instance Binary Coinbase where

    get = do
        v <- getWord32le
        (VarInt len) <- get
        unless (len == 1) $ fail "Coinbase get: Input size is not 1"
        op <- get
        (VarInt cbLen) <- get
        cb <- getByteString (fromIntegral cbLen)
        sq <- getWord32le
        (VarInt oLen) <- get
        os <- replicateM (fromIntegral oLen) get
        lt <- getWord32le
        return $ Coinbase v op cb sq os lt

    put (Coinbase v op cb sq os lt) = do
        putWord32le v
        put $ VarInt 1
        put op
        put $ VarInt $ fromIntegral $ BS.length cb
        putByteString cb
        putWord32le sq
        put $ VarInt $ fromIntegral $ length os
        forM_ os put
        putWord32le lt
