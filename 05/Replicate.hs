import System.IO

myReplicate a b 
    | a > 0 = [b] ++ myReplicate (a-1) b
    | otherwise = []

main = do
    print $ myReplicate 4 0 == [0, 0, 0, 0]
    print $ myReplicate 2 True == [True, True]
    print $ myReplicate 3 "banana" == ["banana", "banana", "banana"]
