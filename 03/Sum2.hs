import System.IO

sum2 a b = a+b

main = do
    a <- readLn :: IO Int
    b <- readLn :: IO Int
    print $ sum2 a b
