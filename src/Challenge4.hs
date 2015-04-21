module Challenge4 where

import qualified Helpers
import qualified Challenge3

main :: IO ()
main = do
  contents <- readFile "challenge-data/4.txt"
  putStrLn . show . Challenge3.topSolution $
    foldr (\line solutions -> ((Challenge3.generateSolutions . Helpers.hexToBytes $ line) ++ solutions)) [] (lines contents)

-- obviously this is not very efficient
