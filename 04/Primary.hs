import System.IO

primary (x:xs) = xs

main = do
    a <- readLn :: IO [Int]
    print $ primary a
