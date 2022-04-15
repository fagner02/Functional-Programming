import Data.List

greater n xs 
    | null xs = []
    | head xs > n = head xs: greater n (tail xs)
    | otherwise = greater n (tail xs)
    
main = do
    a <- readLn :: IO Int
    b <- readLn :: IO [Int]
    print $ greater a b
