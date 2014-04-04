import Data.List

divisors :: Int -> Int -> [Int]
divisors n m = [ x | x <- [1 .. n-1], mod x m == 0]

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

sum35 = sum $ rmdups $ divisors 1000 3 ++ divisors 1000 5

