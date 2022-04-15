import System.IO

perfectSquare' i a 
    | i * i == a = True
    | i * i > a = False
    | otherwise = perfectSquare' (i+1) a

perfectSquare a = perfectSquare' 1 a

main = do
    a <- readLn :: IO Int
    print $ perfectSquare a
