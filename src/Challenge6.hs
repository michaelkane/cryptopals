module Challenge6 where

import Data.Word (Word8)
import Data.ByteString.Lazy.Internal (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Bits as Bits
import qualified Data.ByteString.Base64.Lazy as Base64
import qualified Helpers
import Data.List (sortBy, maximumBy)

-- Assumes the arguments are the same length
hammingDistance :: ByteString -> ByteString -> Int
hammingDistance x y = sum $ map (Bits.popCount) (B.zipWith (Bits.xor) x y)

-- Discards any bytes at the end which would form a block of less than blockSize
blockify :: Int -> ByteString -> [ByteString]
blockify blockSize bytes
  | B.length firstBlock < blockSizeI = []
  | otherwise = firstBlock : blockify blockSize rest
  where
    blockSizeI = fromIntegral blockSize
    (firstBlock, rest) = B.splitAt blockSizeI bytes

-- keySizeScore :: Int -> ByteString -> Double
-- keySizeScore keySize cipher = normalizedScore where
--   blocks = blockify keySize cipher
--   firstBlock = head blocks
--   secondBlock = head . tail $ blocks
--   hammingScore = hammingDistance firstBlock secondBlock
--   normalizedScore = fromIntegral hammingScore / fromIntegral keySize

-- Note: By following the exact method in the text I was not guessing the
-- correct keysize (should be 29).  That method computed the normalized hamming
-- distance between the first 2 blocks for a given keysize.  (See the commented
-- out keySizeScore above.)  Instead I got the correct result by computing the
-- normalized hamming distance between ALL consecutive blocks and averaging.

joinWith :: (a -> a -> b) -> [a] -> [b]
joinWith _ [] = []
joinWith _ [_] = []
joinWith f (x1:x2:xs) = f x1 x2 : joinWith f xs

keySizeScore :: Int -> ByteString -> Double
keySizeScore keySize cipher = sum normalizedScores / fromIntegral (length blocks) where
  blocks = blockify keySize cipher
  normalizedScores = joinWith (\a b -> fromIntegral (hammingDistance a b) / fromIntegral keySize) blocks

rankKeySizes :: [Int] -> ByteString -> [Int]
rankKeySizes keySizes cipher = map fst $ sortBy sortFn scoredKeySizes where
  scoredKeySizes = map (\size -> (size, keySizeScore size cipher)) keySizes
  sortFn = \a b -> compare (snd a) (snd b)

solveSingleByteXor :: ByteString -> Word8
solveSingleByteXor cipher = fst $ maximumBy sortFn scoredSolutions where
  possibleKeys = [0..255] :: [Word8]
  decryptWith key = B.map (Bits.xor key) cipher
  score = Helpers.englishness . B.toStrict . decryptWith
  scoredSolutions = [(key, score key) | key <- possibleKeys]
  sortFn = \a b -> compare (snd a) (snd b)

decrypt :: ByteString -> ByteString -> ByteString
decrypt key cipher = B.pack $ B.zipWith (Bits.xor) repeatedKey cipher
  where repeatedKey = B.concat $ repeat key

main :: IO ()
main = do
  cipherB64 <- BC.readFile "challenge-data/6.txt"
  let cipher = Base64.decodeLenient cipherB64
  let keySize = head $ rankKeySizes [2..40] cipher
  let blocks = blockify keySize cipher
  let transposedBlocks = B.transpose blocks
  let solution = B.pack $ map solveSingleByteXor transposedBlocks
  BC.putStrLn $ decrypt solution cipher
  putStrLn $ "(Decrypted with key: " ++ show solution ++ ")"
