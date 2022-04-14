import Data.List

intercal xs ys
    | null xs = ys
    | null ys = xs
    | otherwise = [head xs, head ys] ++ intercal (tail xs) (tail ys)
    
main = do
    a <- readLn :: IO [Int]
    b <- readLn :: IO [Int]
    print $ intercal a b
