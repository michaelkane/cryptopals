module Challenge5 where

import Data.ByteString.Lazy.Internal (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Bits as Bits
-- import qualified Helpers

encrypt :: ByteString -> ByteString -> ByteString
encrypt key plaintext = B.pack $ B.zipWith (Bits.xor) repeatedKey plaintext
  where repeatedKey = BC.concat $ repeat key

-- Read key from command line and plaintext from stdin
main :: IO ()
main = do
   BC.interact $ encrypt (BC.pack "key")
