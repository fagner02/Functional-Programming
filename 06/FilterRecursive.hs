import Data.List

filterRecursive' x y 
    | x y = [y]
    | otherwise = []

filterRecursive x xs 
    | null xs = []
    | otherwise = filterRecursive' x (head xs) ++ filterRecursive x (tail xs)
    
main = do
    print $ filterRecursive (>5) [0..10] == [6,7,8,9,10]
    print $ filterRecursive (odd) [0..10] == [1,3,5,7,9]
