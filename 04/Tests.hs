import Data.List

sumAll xs = foldl (+) 0 xs

smaller xs = foldl (\x y -> case () of 
  _ | x <= y -> x
    | y <= x -> y) (head xs) (tail xs)
    
exists x xs = foldl (\a b -> (b == x) || a) False xs

odds xs = foldl (\x y -> case () of
   _| mod y 2 == 0 -> x
    | otherwise -> y : x) [] xs 
    
findRepeated xs = snd (foldl (\x y -> 
    if y == fst x then (y, (snd x)+ 1)
    else (y, snd x)) (head xs, 0) (tail xs))

prime x = not (foldl (\a b -> (mod x b == 0) || a) False [2..x-1])

main = do
    print $ sumAll [1,2,3]
    print $ smaller [2,1,3]
    print $ exists 5 [6,4,4,6,5,7]
    print $ odds [6,5,2,3,7,2,8]
    print $ findRepeated [6,6,6,4,3,3,3,4,4]
    print $ prime 66
