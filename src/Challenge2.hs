module Challenge2 where

import qualified Helpers
import qualified Data.ByteString as B
import qualified Data.Bits as Bits

-- "\FS\SOH\DC1\NUL\US\SOH\SOH\NUL\ACK\SUB\STXKSSP\t\CAN\FS"
input1 :: B.ByteString
input1 = Helpers.hexToBytes "1c0111001f010100061a024b53535009181c"

-- "hit the bull's eye"
input2 :: B.ByteString
input2 = Helpers.hexToBytes "686974207468652062756c6c277320657965"

-- "the kid don't play"
answer :: String
answer = Helpers.bytesToHex . B.pack $ B.zipWith (Bits.xor) input1 input2
