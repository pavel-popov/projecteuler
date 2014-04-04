

sum_of_squares n = sum $ map (^2) [1..n]
square_of_sum n = (sum [1..n])^2

{-
*Main> (square_of_sum 100) - (sum_of_squares 100)
25164150
-}

