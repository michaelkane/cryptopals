module Challenge4 where

import qualified Helpers
import qualified Challenge3

main :: IO ()
main = do
  contents <- readFile "challenge-data/4.txt"
  putStrLn . show . Challenge3.topSolution $ map (Challenge3.topSolution . Challenge3.generateSolutions . Helpers.hexToBytes) (lines contents)


answer :: String
answer = "TODO"
