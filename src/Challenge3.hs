module Challenge3 where

import qualified Helpers
import qualified Data.Bits as Bits
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as B
import GHC.Word (Word8)
import qualified Data.List as List
import qualified Data.Ord as Ord

input :: ByteString
input = Helpers.hexToBytes "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

keys :: [Word8]
keys = [0..255]

-- ciphertext -> key -> plaintext
decrypt :: ByteString -> Word8 -> ByteString
decrypt ciphertext key = B.map (Bits.xor key) ciphertext

-- [(plaintext, key)]
generateSolutions :: ByteString -> [(ByteString, Word8)]
generateSolutions ciphertext = [(decrypt ciphertext k, k) | k <- keys]

-- [((plaintext, key), score)]
scoreSolutions :: [(ByteString, Word8)] -> [((ByteString, Word8), Rational)]
scoreSolutions solutions = map (\x -> (x, Helpers.englishness (fst x))) solutions

-- (plaintext, key)
topSolution :: [(ByteString, Word8)] -> (ByteString, Word8)
topSolution solutions = fst topScoredSolution
  where
    topScoredSolution = List.maximumBy cmp (scoreSolutions solutions)
    cmp = \x y -> Ord.compare (snd x) (snd y)

answer :: String
answer = "Cyphertext: " ++ show (Helpers.bytesToHex input) ++ "\n" ++
         "Plaintext: " ++ show (fst solution) ++ "\n" ++
         "Key: " ++ show (B.pack [snd solution])
  where
    solution = topSolution . generateSolutions $ input
