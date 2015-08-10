{-# Language OverloadedStrings #-}
module Challenge10 where

-- Implement CBC mode
-- CBC mode is a block cipher mode that allows us to encrypt irregularly-sized
-- messages, despite the fact that a block cipher natively only transforms
-- individual blocks.

-- In CBC mode, each ciphertext block is added to the next plaintext block
-- before the next call to the cipher core.

-- The first plaintext block, which has no associated previous ciphertext block,
-- is added to a "fake 0th ciphertext block" called the initialization vector,
-- or IV.

-- Implement CBC mode by hand by taking the ECB function you wrote earlier,
-- making it encrypt instead of decrypt (verify this by decrypting whatever you
-- encrypt to test), and using your XOR function from the previous exercise to
-- combine them.

-- The file here is intelligible (somewhat) when CBC decrypted against "YELLOW
-- SUBMARINE" with an IV of all ASCII 0 (\x00\x00\x00 &c)

import Pipes
import qualified Pipes.ByteString as PB

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Data.Word (Word8)
import qualified Data.Bits as Bits

import Crypto.Cipher.AES128 (AESKey128)
import qualified Crypto.Cipher.AES128 as AES128


pkcs7 :: Int -> ByteString -> ByteString
pkcs7 blockSize bs = B.append bs padding
  where
    paddingLength = mod blockSize (B.length bs)
    paddingByte = fromIntegral paddingLength :: Word8
    padding = B.replicate paddingLength (paddingByte)

xor :: ByteString -> ByteString -> ByteString
xor a b = B.pack $ B.zipWith (Bits.xor) a b


-- Assumes the given 'previous' block is already padded to blockSize (i.e. make
-- sure the IV is blockSize!)
cbcEncrypt :: (Monad m) =>
              Int ->
              (ByteString -> ByteString) ->
              ByteString ->
              Pipe ByteString ByteString m r
cbcEncrypt blockSize encryptFn iv = do
  plainBlock <- await
  let paddedPlainBlock = pkcs7 blockSize plainBlock
  let xoredBlock = xor iv paddedPlainBlock
  let cipherBlock = encryptFn xoredBlock
  yield cipherBlock
  cbcEncrypt blockSize encryptFn cipherBlock


cbcDecrypt :: (Monad m) =>
              Int ->
              (ByteString -> ByteString) ->
              ByteString ->
              Pipe ByteString ByteString m r
cbcDecrypt blockSize decryptFn iv = do
  cipherBlock <- await
  let xoredBlock = decryptFn cipherBlock
  let plainBlock = xor iv xoredBlock
  yield plainBlock
  cbcDecrypt blockSize decryptFn cipherBlock

-- e.g. cat challenge-data/10.txt | base64 -d | ./dist/build/cryptopals/cryptopals
-- Switch between encrypting and decrypting in the code
main :: IO ()
main =
  let
    blockSize = 16 -- (AES blocksize == 128 bits == 16 bytes)
    Just key = AES128.buildKey "YELLOW SUBMARINE" :: Maybe AESKey128
    -- encryptor = AES128.encryptBlock key
    decryptor = AES128.decryptBlock key
    iv = BC.replicate blockSize '0'
  in
    runEffect $
      (PB.chunksOf' blockSize PB.stdin) >->
      cbcDecrypt blockSize decryptor iv >->
      PB.stdout

-- Todo: make a proper cmdline program (accept a key / iv / encrpyt - decrypt).
-- Todo: make a pipe which splits into blockSize chunks and does padding at end.
