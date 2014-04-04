isPolindrome :: Integer -> Bool
isPolindrome x = reverse x' == x'
    where x' = show x

polindromes = [ x*y | x <- [100..999], y <- [100..999], isPolindrome (x*y) ]

{-
*Main> maximum polindromes
906609
-}

