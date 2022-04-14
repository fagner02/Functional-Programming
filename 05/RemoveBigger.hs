import Data.List

removeBigger (xs)
    | null xs = []
    | (length xs) == 2 = [min (xs!!0) (xs!!1)]
    | (xs!!0) >= (xs!!1) = [xs!!1] ++ removeBigger ([xs!!0] ++ drop 2 xs)
    | (xs!!1) >= (xs!!0) = [xs!!0] ++ removeBigger (tail xs)
    
main = do
    b <- readLn :: IO [Int]
    print $ removeBigger b
