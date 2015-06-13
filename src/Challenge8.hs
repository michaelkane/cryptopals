-- Detect AES in ECB mode In this file are a bunch of hex-encoded ciphertexts.
-- One of them has been encrypted with ECB.
-- Detect it.
-- Remember that the problem with ECB is that it is stateless and deterministic;
-- the same 16 byte plaintext block will always produce the same 16 byte
-- ciphertext.

module Challenge8 where

import Data.ByteString.Lazy.Internal (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Base16 as Base16
import Challenge6 (blockify, hammingDistance)
import Data.List (sortBy)

blockSize :: Int
blockSize = 16

decodeLenientB16 :: ByteString -> ByteString
decodeLenientB16 = BC.fromStrict . fst . Base16.decode . BC.toStrict

totalHammingDistance :: [ByteString] -> Int
totalHammingDistance blocks = sum $ map (\(b1, b2) -> hammingDistance b1 b2) blockCombinations
  where
    blockCombinations = [(b1, b2) | b1 <- blocks, b2 <- blocks] -- ah.. ??

averageHammingDistance :: [ByteString] -> Double
averageHammingDistance blocks = fromIntegral (totalHammingDistance blocks) / fromIntegral (length blocks)

-- Lower score represents a ciphertext that has blocks more similar to each
-- other... so more likely to be ECB as similarity of blocks will reflect
-- patterns in the plaintext (which would be expected for non-random plaintext)
score :: ByteString -> Double
score ciphertext = averageHammingDistance $ blockify blockSize ciphertext

main :: IO ()
main = do
  fileB16 <- BC.readFile "challenge-data/8.txt"
  let ciphertextsB16 = BC.lines fileB16
  let withScores = map (\x -> (x, score . decodeLenientB16 $ x)) ciphertextsB16
  let sortedWithScores = sortBy (\x y -> snd x `compare` snd y) withScores
  print $ head sortedWithScores
