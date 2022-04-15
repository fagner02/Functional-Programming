import Data.List

removeBiggest' n (x:xs)
    | x == n = xs
    | otherwise = x:(removeBiggest' n xs)

removeBiggest xs = removeBiggest' (maximum xs) xs
    
main = do
    b <- readLn :: IO [Int]
    print $ removeBiggest b
