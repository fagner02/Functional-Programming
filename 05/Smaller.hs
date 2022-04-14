import Data.List

smaller n xs 
    | null xs = []
    | head xs < n = [head xs] ++ smaller n (tail xs)
    | otherwise = smaller n (tail xs)
    
main = do
    a <- readLn :: IO Int
    b <- readLn :: IO [Int]
    print $ smaller a b
