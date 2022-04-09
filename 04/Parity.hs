import System.IO
import Data.List

parity xs = (mod (length $ filter (==True) $ sort xs) 2) == 1
  
main = do
    a <- readLn :: IO [Bool]
    print $ parity a
