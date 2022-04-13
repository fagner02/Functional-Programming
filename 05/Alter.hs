import Data.List

alterRecursive n 
    | n == 0 = []
    | otherwise = alterRecursive (n-1) ++ [n,-n]
  
main = do
    a <- readLn :: IO Int
    print $ alterRecursive a
