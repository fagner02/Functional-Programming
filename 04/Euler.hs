import System.IO

eulerNum a = sum [x | x <- [1..a-1], mod x 3 == 0 || mod x 5 == 0]

main = do
    a <- readLn :: IO Int
    print $ eulerNum a
