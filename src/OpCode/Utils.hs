module OpCode.Utils where

import Crypto.Hash

import qualified Data.ByteArray (convert)
import Data.ByteString hiding (unfoldr, foldl')
import qualified Data.ByteString as B
import Data.Monoid
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits
import Data.Word
import Numeric.Natural
import Data.ByteString.Lazy (toStrict, fromStrict)

import Data.List (unfoldr, foldl')

integerToEVM256 :: Natural -> ByteString
integerToEVM256 = toStrict . runPut . integerToEVM256'

evm256ToInteger :: ByteString -> Natural
evm256ToInteger bs = runGet evm256ToInteger' (fromStrict bs)

integerToEVM256' :: Natural -> PutM ()
integerToEVM256' n = mapM_ put (Prelude.reverse $ unroll n) -- unroll the bytes

evm256ToInteger' :: Get Natural
evm256ToInteger' = do
    empty <- isEmpty
    if empty
        then pure 0
        else do
            bytes <- getBytes' []
            pure $! roll bytes

getBytes' cs = do
    empty <- isEmpty
    if empty
        then pure cs
        else do
            byte <- get :: Get Word8
            getBytes' (byte:cs)

unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: (Integral a, Bits a) => [Word8] -> a
roll   = foldl' unstep 0 . Prelude.reverse
    where
        unstep a b = a `shiftL` 8 .|. fromIntegral b

keccak256Bytes :: B.ByteString -> B.ByteString
keccak256Bytes = Data.ByteArray.convert . keccak256

keccak256 :: B.ByteString -> Digest Keccak_256
keccak256 bs = hash bs
