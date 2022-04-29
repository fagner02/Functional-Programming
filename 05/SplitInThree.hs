import Data.List

split n xs = (take n xs, xs!!n, drop (n+1) xs)

splitAtRecursive' n xs 
    | length xs < 2 = ([],[])
    | (length xs) - n == 1 = (((head xs): fst (splitAtRecursive' (n-1) list)), [])
    | n == 0 = ([],(xs!!(n+1)):snd (splitAtRecursive' 0 ((head xs) :(drop 2 xs))))
    | otherwise = ((head xs):fst (splitAtRecursive' (n-1) list), (xs!!(n+1)):snd (splitAtRecursive' (n-1) list))
    where list = (tail (take (n+1) xs))++(drop (n+2) xs)
    
splitAtRecursive n xs = (fst (splitAtRecursive' n xs), xs!!n, snd (splitAtRecursive' n xs))

splitAtFold' n xs = foldl (\x y -> if length (fst x) == n then (fst x, (snd x)++[y])
else ((fst x)++[y], snd x)) ([],[]) ((take n xs)++(drop (n+1) xs))

splitAtFold n xs = (fst (splitAtFold' n xs),xs!!n, snd (splitAtFold' n xs))
main = do
    a <- readLn :: IO Int 
    b <- readLn :: IO [Int]
    print $ split a b
    print $ splitAtRecursive a b
    print $ splitAtFold a b
