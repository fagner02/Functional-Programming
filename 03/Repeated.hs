import System.IO
import Data.List

repeated a b c = 
    sum (filter (>1) (map length (group (sort xs))))
    where xs = [a,b,c]

main = do
    a <- readLn :: IO Int
    b <- readLn :: IO Int
    c <- readLn :: IO Int
    print $ repeated a b c
