import System.IO

split xs n = (take n xs, drop n xs)

main = do
    a <- readLn :: IO [Int]
    b <- readLn :: IO Int
    print $ split a b
