import System.IO

maxElement (x:xs) 
    | null xs = x
    | otherwise =
          if (x > head xs)
          then maxElement (x : tail xs)
          else maxElement xs

main = do
    a <- readLn :: IO [Int]
    print $ maxElement a
