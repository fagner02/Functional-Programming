import Data.List

sequenceNum a b
    | a == 0 = []
    | otherwise = (sequenceNum (a-1) b) ++ [b+a-1]

main = do
    a <- readLn :: IO Int
    b <- readLn :: IO Int
    print $ sequenceNum a b
