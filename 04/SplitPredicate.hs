import System.IO

splitPred f xs = (filter f xs, filter c xs)
    where
       c:: Int -> Bool
       c a = not (f a)


main = do
    b <- readLn :: IO [Int]
    print $ splitPred odd b
   
