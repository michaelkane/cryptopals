module Helpers where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as Base16

hexToBytes :: String -> BC.ByteString
hexToBytes = fst . Base16.decode . BC.pack

bytesToHex :: BC.ByteString -> String
bytesToHex = BC.unpack . Base16.encode
