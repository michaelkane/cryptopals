module Challenge5 where

import Data.ByteString.Lazy.Internal (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Bits as Bits
import Options.Applicative

encrypt :: ByteString -> ByteString -> ByteString
encrypt key plaintext = B.pack $ B.zipWith (Bits.xor) repeatedKey plaintext
  where repeatedKey = B.concat $ repeat key

encryptStdinWithKey :: String -> IO ()
encryptStdinWithKey key = B.interact $ encrypt (BC.pack key)


keyOpt :: Parser String
keyOpt = strOption
      ( long "key"
        <> short 'k'
        <> metavar "KEY"
        <> help "Encryption key" )

description :: String
description = "Encode stdin using KEY via repeating-key XOR.  (Note that\
               \ output is in bytes, so to convert to hex try piping to\
               \`xxd -p`)"

main :: IO ()
main = execParser opts >>= encryptStdinWithKey
  where
    opts = info (helper <*> keyOpt)
           ( fullDesc
             <> progDesc description
             <> header "repeating-key XOR encryption" )


-- -- Read key from command line and plaintext from stdin
-- main :: IO ()
-- main = do
--    B.interact $ encrypt (B.pack [2,34,4,23])
