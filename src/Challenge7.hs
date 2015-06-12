{-# LANGUAGE OverloadedStrings #-}

module Challenge7 where

import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Base64.Lazy as Base64
import Crypto.Cipher.AES128 (AESKey128)
import Crypto.Classes (buildKey, unEcbLazy)

main :: IO ()
main = do
  ciphertextB64 <- BC.readFile "challenge-data/7.txt"
  let ciphertext = Base64.decodeLenient ciphertextB64
  let Just key = buildKey "YELLOW SUBMARINE" :: Maybe AESKey128
  BC.putStrLn $ unEcbLazy key ciphertext
