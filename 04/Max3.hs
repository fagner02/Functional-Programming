import System.IO

max3 a b c = maximum [a,b,c]

main = do
    a <- readLn :: IO Int
    b <- readLn :: IO Int
    c <- readLn :: IO Int
    print $ max3 a b c
