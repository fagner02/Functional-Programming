rRight a b
    | a == 0 = b
    | otherwise = rRight (a-1) (last b : (init b))

main = do
    a <- readLn :: IO Int
    b <- getLine
    print $ rRight a b
