import System.IO

exists a xs = elem a xs

existsFilter a xs = filter (==a) xs /= []

existsRecursive a (x:xs)
    | xs == [] = False
    | a == x = True
    | otherwise = existsRecursive a xs


main = do
    a <- readLn :: IO Int
    b <- readLn :: IO [Int]
    print $ exists a b
    print $ existsFilter a b
    print $ existsRecursive a b
