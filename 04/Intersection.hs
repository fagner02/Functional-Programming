import System.IO
import Data.List

intersec xs ys = [x | x <- xs, y <- ys, x == y]
  
main = do
    a <- readLn :: IO [Int]
    b <- readLn :: IO [Int]
    print $ intersec a b
