module Helpers where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as Base16

hexToBytes :: String -> ByteString
hexToBytes = fst . Base16.decode . BC.pack

bytesToHex :: ByteString -> String
bytesToHex = BC.unpack . Base16.encode

-- http://en.algoritmy.net/article/40379/Letter-frequency-English
englishCharacterFrequency :: Char -> Rational
englishCharacterFrequency x
  | x == 'a'  = 0.08167
  | x == 'b'  = 0.01492
  | x == 'c'  = 0.02782
  | x == 'd'  = 0.04253
  | x == 'e'  = 0.12702
  | x == 'f'  = 0.02228
  | x == 'g'  = 0.02015
  | x == 'h'  = 0.06094
  | x == 'i'  = 0.06966
  | x == 'j'  = 0.00153
  | x == 'k'  = 0.00772
  | x == 'l'  = 0.04025
  | x == 'm'  = 0.02406
  | x == 'n'  = 0.06749
  | x == 'o'  = 0.07507
  | x == 'p'  = 0.01929
  | x == 'q'  = 0.00095
  | x == 'r'  = 0.05987
  | x == 's'  = 0.06327
  | x == 't'  = 0.09056
  | x == 'u'  = 0.02758
  | x == 'v'  = 0.00978
  | x == 'w'  = 0.02360
  | x == 'x'  = 0.00150
  | x == 'y'  = 0.01974
  | x == 'z'  = 0.00074
  | otherwise = 0

englishCharacters :: [Char]
englishCharacters = ['a'..'z']

characterFrequency :: Char -> ByteString -> Rational
characterFrequency c s
  | BC.null s = 0
  | otherwise = fromIntegral (BC.count c s) / fromIntegral (BC.length s)

englishness :: ByteString -> Rational
englishness s = sum $ map freqDiff englishCharacters
  where freqDiff c = abs (characterFrequency c s - englishCharacterFrequency c)
