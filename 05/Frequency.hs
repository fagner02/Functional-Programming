import System.IO

frequency a xs
    | null xs = 0
    | (head xs) == a = 1 + frequency a (tail xs)
    | otherwise = frequency a (tail xs)

main = do
    a <- readLn :: IO Int
    b <- readLn :: IO [Int]
    print $ frequency a b
