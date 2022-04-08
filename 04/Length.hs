import System.IO

getLength xs = sum $ map (\x -> 1) xs

getLengthRecursive xs
    | null xs = 0
    | otherwise = 1 + getLengthRecursive (tail xs)

getLengthFold xs = foldl (\x y -> y*0+1+x) 0 xs 

main = do
    b <- readLn :: IO [Int]
    print $ getLengthRecursive b
    print $ getLengthFold b
    print $ getLength b
