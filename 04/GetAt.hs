import System.IO

at n xs = xs!!i
    where i = mod (n+(length xs)) (length xs)

atRecursive n xs
    | i == 0 = head xs
    | 0 == length xs = head xs
    | otherwise = atRecursive (i-1) (tail xs)
    where i = mod (n+(length xs)) (length xs)

main = do
    a <- readLn :: IO Int
    b <- readLn :: IO [Int]
    print $ atRecursive a b
    print $ at a b
