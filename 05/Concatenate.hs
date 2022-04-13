import Data.List

concatenate xs ys = concat [xs, ys]

concatenateRecursive xs ys 
    | null xs = ys
    | null ys = xs
    | otherwise = 
         concat [ 
             concat [[head xs], concatenate (tail xs) []], 
             concat [[head ys], concatenate (tail ys) []]
         ]
  
main = do
    a <- readLn :: IO [Int]
    b <- readLn :: IO [Int]
    print $ concatenate a b
    print $ concatenateRecursive a b
