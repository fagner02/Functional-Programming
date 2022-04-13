import Data.List

reverseRecursive xs 
    | null xs = []
    | otherwise = [last xs] ++ reverseRecursive (init xs)
  
main = do
    a <- readLn :: IO [Int]
    print $ reverseRecursive a
