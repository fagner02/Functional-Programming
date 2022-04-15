import Data.List

concatenate xs ys = concat [xs, ys]

concatenateRecursive xs ys 
    | null xs = ys
    | otherwise = concatenate (init xs) (last xs : ys)
  
main = do
    a <- readLn :: IO [Int]
    b <- readLn :: IO [Int]
    print $ concatenate a b
    print $ concatenateRecursive a b
