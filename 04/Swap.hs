import System.IO

swap xs a b = map (\x -> case () of 
                       _ | x == xs!!a -> xs!!b
                         | x == xs!!b -> xs!!a
                         | otherwise -> x) xs
  
main = do
    a <- readLn :: IO [Int]
    b <- readLn :: IO Int
    c <- readLn :: IO Int
    print $ swap a b c
