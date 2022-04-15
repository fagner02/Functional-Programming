import Data.List

line' n m 
    | m > n = 0
    | n == m = 1
    | otherwise = m + line' n (m+1)

line n = [limit..limit+n-1]
         where limit = line' n 1

triangle n
    | n == 0 = []
    | otherwise = triangle (n-1) ++ [line n]
  
main = do
    a <- readLn :: IO Int
    print $ triangle a
