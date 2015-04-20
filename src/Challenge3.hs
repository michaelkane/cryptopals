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
allSolutions :: [(ByteString, Word8)]
allSolutions = [(decrypt input k, k) | k <- keys]

-- [(plaintext, key, score)]
scoredSolutions :: [(ByteString, Word8, Rational)]
scoredSolutions = map (\x -> (fst x, snd x, Helpers.englishness (fst x))) allSolutions

-- (plaintext, key, score)
topSolution :: (ByteString, Word8, Rational)
topSolution = head $ List.sortBy cmp scoredSolutions
  where cmp = \x y -> Ord.compare (third x) (third y)
        third = \(_,_,x) -> x

answer :: String
answer = "Cyphertext: " ++ show (Helpers.bytesToHex input) ++ "\n" ++
         "Plaintext: " ++ show (first topSolution) ++ "\n" ++
         "Key: " ++ show (B.pack [second topSolution])
  where first = \(x,_,_) -> x
        second = \(_,x,_) -> x
