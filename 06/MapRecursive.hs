import Data.List

mapRecursive x xs 
    | null xs = []
    | otherwise = (x (head xs)) : mapRecursive x (tail xs)

main = do
    print $ mapRecursive (+1) [1, 2, 3] == [2, 3, 4]
    print $ mapRecursive (odd) [6,2,1] == [False, False, True]
