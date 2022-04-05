import System.IO

interior xs = 
   take ((length xs) -2) (drop 1 xs)

main = do
    a <- readLn :: IO [Int]
    print $ interior a
