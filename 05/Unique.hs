import Data.List

uniqueFilter n xs = (length (filter (==n) xs)) == 1

uniqueRecursive' n xs
    | null xs = 0
    | (head xs) == n = 1 + uniqueRecursive' n (tail xs)
    | otherwise = uniqueRecursive' n (tail xs)

uniqueRecursive n xs = (uniqueRecursive' n xs) == 1
    
main = do
    a <- readLn :: IO Int
    b <- readLn :: IO [Int]
    print $ uniqueRecursive a b
    print $ uniqueFilter a b
