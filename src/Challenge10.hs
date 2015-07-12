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

import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Bits as Bits
import Crypto.Cipher.AES128 (AESKey128)
import Crypto.Classes (buildKey, encryptBlock, zeroIV, IV)

pad :: Int -> ByteString -> ByteString
pad blockSize bs = B.append bs padding
  where
    paddingLength = mod blockSize (B.length bs)
    paddingByte = fromIntegral paddingLength :: Word8
    padding = B.replicate paddingLength (paddingByte)

-- Pads the last block using PKCS#7 if less than blockSize
blockify :: Int -> ByteString -> [ByteString]
blockify blockSize bytes
  | B.length firstBlock < blockSizeI = [pad blockSize firstBlock]
  | otherwise = firstBlock : blockify blockSize rest
  where
    blockSizeI = fromIntegral blockSize
    (firstBlock, rest) = B.splitAt blockSizeI bytes

xor :: ByteString -> ByteString -> ByteString
xor = (B.pack .) . (B.zipWith Bits.xor)

-- aes128BlockCipher :: ByteString -> ByteString -> ByteString
-- aes128BlockCipher key plaintext = undefined
--   where Just aes128Key = buildKey 

encryptAES128Cbc :: AESKey128 -> ByteString -> ByteString -> ByteString
encryptAES128Cbc key iv plaintext = undefined

main :: IO ()
main = do
  let Just key = buildKey "YELLOW SUBMARINE" :: Maybe AESKey128
  let iv = zeroIV :: IV AESKey128
  let plaintext = "I'd love you to encrypt me!"
  print ("hi" :: String)
  --print $ encryptAES128Cbc key iv plaintext


-- import qualified Data.ByteString.Lazy.Char8 as BC
-- import qualified Data.ByteString.Base64.Lazy as Base64
-- import Crypto.Cipher.AES128 (AESKey128)
-- import Crypto.Classes (buildKey, unEcbLazy)

-- main :: IO ()
-- main = do
--   ciphertextB64 <- BC.readFile "challenge-data/7.txt"
--   let ciphertext = Base64.decodeLenient ciphertextB64
--   let Just key = buildKey "YELLOW SUBMARINE" :: Maybe AESKey128
--   BC.putStrLn $ unEcbLazy key ciphertext
