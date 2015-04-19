module Challenge1 where

import qualified Helpers
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base64 as Base64

-- "I'm killing your brain like a poisonous mushroom"
input :: BC.ByteString
input = Helpers.hexToBytes "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

answer :: String
answer = BC.unpack $ Base64.encode $ input
