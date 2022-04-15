import Data.List

accumulateList' n xs 
    | null xs = []
    | otherwise = m : accumulateList' m (tail xs)
    where m = head xs + n

accumulateList xs = head xs : accumulateList' (head xs) (tail xs)
    
[4 ]
main = do
    a <- readLn :: IO [Int]
    print $ accumulateList a
