import System.IO

fatorial n = product [1..n]

main = do
    a <- readLn :: IO Int
    print $ fatorial a
