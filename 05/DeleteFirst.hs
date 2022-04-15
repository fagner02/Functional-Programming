import Data.List

deleteFirst n xs
    | null xs = []
    | (head xs) == n = tail xs 
    | otherwise = (head xs) : deleteFirst n (tail xs) 


main = do
    a <- readLn :: IO Int
    b <- readLn :: IO [Int]
    print $ deleteFirst a b
