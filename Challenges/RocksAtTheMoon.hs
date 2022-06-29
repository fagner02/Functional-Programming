import Control.Monad
import Data.List
import Data.Maybe

ress x y
  | x < 10 || y < 10 = -1
  | otherwise = abs (x - y)

process xs
  | not (any (>= 0) search) = "sem ganhador"
  | otherwise = show $ fromJust $ elemIndex (minimum $ filter (>= 0) search) search
  where
    search = map ((\x -> ress (head x) (x !! 1)) . (map read . words)) xs

main = do
  size <- readLn :: IO Int
  nums <- replicateM size getLine
  print $ process nums
