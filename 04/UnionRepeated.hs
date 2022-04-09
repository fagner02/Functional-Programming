import System.IO
import Data.List

unionRepeated xs ys = map head $ group $ sort $ xs ++ ys

main = do
    a <- readLn :: IO [Int]
    b <- readLn :: IO [Int]
    print $ unionRepeated a b
