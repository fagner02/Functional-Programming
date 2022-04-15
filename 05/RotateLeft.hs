rLeft a b
    | a == 0 = b
    | otherwise = rLeft (a-1) ((tail b)++[head b])

main = do
    a <- readLn :: IO Int
    b <- getLine
    print $ rLeft a b
