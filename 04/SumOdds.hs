import System.IO

sumOdd xs = sum (filter odd xs)
    
sumOddCompList xs = sum [x | x <- xs, odd x]

main = do
    b <- readLn :: IO [Int]
    print $ sumOdd b
    print $ sumOddCompList b
   
