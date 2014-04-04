
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs :: Int -> [Int]
fibs n = take n $ map fib [1..]

{-
fibs_even :: Int -> [Int]
fibs_even n = take n $ filter (\x -> (even x) && (x<=4000000)) $ map fib [1..4000000]
-}


fibs_even :: Int -> [Int]
fibs_even h = [ x | x <- takeWhile (<=h) (map fib [1..]), even x]

sum_fibs = sum . fibs_even

{-
*Main> sum_fibs 4000000
4613732
-}


