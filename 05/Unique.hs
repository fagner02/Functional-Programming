import Data.List

uniqueFilter n xs = (length (filter (==n) xs)) == 1

uniqueRecursive n xs 
    | null xs = False
    | ((length (group xs)) == 1) = (length xs) == 1
    | not ((head xs) == n) = uniqueRecursive n (tail xs)
    | n == (head xs) = uniqueRecursive n ((tail xs) ++ [n])

main = do
    a <- readLn :: IO Int
    b <- readLn :: IO [Int]
    print $ uniqueRecursive a b
    print $ uniqueFilter a b
