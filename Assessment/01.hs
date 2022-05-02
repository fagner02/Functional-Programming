import Data.List

-- 1 ----------
index n xs
  | null xs = -1
  | n == head xs = 0
  | otherwise = case () of
    _
      | i == 0 -> -1
      | otherwise -> i
  where
    i = 1 + index n (tail xs)

-- 2 ----------
indexFold n xs 
  | snd i = fst i
  | otherwise = -1
  where
    i = foldl (\x y -> case () of
        _
          | snd x -> x 
          | otherwise -> if y == n 
                         then (fst x, True) 
                         else (fst x + 1, False)
              ) (0, False) xs

-- 3 ----------
triangle a b c = (a + b > c) && (b + c > a) && (a + c > b)

-- 4 ----------
polygon' i xs
  | null xs = False
  | i + 1 == length xs = xs !! i < sum (delete (xs !! i) xs)
  | otherwise = xs !! i < sum (delete (xs !! i) xs) && polygon' (i + 1) xs

polygon xs = polygon' 0 xs

polygonMaximum xs 
  | null xs = False
  | otherwise = m < sum xs - m
  where m = maximum xs

-- 5 ----------
gather2 a b xs
  | length xs == 1 = [[a, b, head xs]]
  | otherwise = [a, b, head xs] : gather2 a b (tail xs)

gather1 a xs
  | length xs == 2 = gather2 a (head xs) (tail xs)
  | otherwise = gather2 a (head xs) (tail xs) ++ gather1 a (tail xs)

gather xs
  | length xs < 3 = []
  | length xs == 3 = gather1 (head xs) (tail xs)
  | otherwise = gather1 (head xs) (tail xs) ++ gather (tail xs)

toothpicks xs = map (\x -> (head (head x), head x !! 1, head x !! 2)) $ group $ sort [sort [x, y, z] | x <- xs, y <- delete x xs, z <- delete y (delete x xs), triangle x y z]

toothpicksGather xs = [(head x, x !! 1, x !! 2) | x <- list, triangle (head x) (x !! 1) (x !! 2)]
  where
    list = gather xs

main = do
  print "1 - index"
  print $ index 1 []
  print $ index 2 [1, 2, 3, 4, 5, 6, 7]
  print $ index 4 [1, 2, 3, 4, 5, 6, 4]
  print $ index 7 [1, 2, 3, 4, 5, 6, 7]
  print $ index 8 [1, 2, 3, 4, 5, 6, 7]
  print $ index 15 [1, 2, 3, 4, 5, 6, 7]
  print "2 - index fold"
  print $ indexFold 2 []
  print $ indexFold 2 [1, 2, 3, 4, 5, 6, 7]
  print $ indexFold 4 [1, 2, 3, 4, 5, 6, 4]
  print $ indexFold 7 [1, 2, 3, 4, 5, 6, 7]
  print $ indexFold 8 [1, 2, 3, 4, 5, 6, 7]
  print $ indexFold 15 [1, 2, 3, 4, 5, 6, 7]
  print "3 - triangle"
  print $ triangle 1 2 3
  print $ triangle 2 2 3
  print $ triangle 7 2 3
  print "4 - polygon"
  print $ polygon []
  print $ polygon [2, 3, 4, 9]
  print $ polygon [2, 3, 9, 3]
  print $ polygon [2, 3, 6, 3]
  print $ polygonMaximum [3,2]
  print $ polygonMaximum [8, 3, 9, 3]
  print $ polygonMaximum [8, 3, 9, 3, 25]
  print $ polygonMaximum [8, 3, 9, 6, 25]
  print "5 - toothpicks"
  print $ toothpicks []
  print $ toothpicks [1, 5, 6, 7]
  print $ toothpicks [1, 2, 5, 6, 7]
  print $ toothpicksGather [2, 3, 5, 12, 15, 20]
