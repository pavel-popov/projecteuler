

foldl' f z []     = z
foldl' f z (x:xs) = let z' = z `f` x
                    in seq z' $ foldl' f z' xs

-- first attempt
isDivisable x xs = length [y | y <- xs, x `mod` y == 0 ] == length xs
minDivisable xs = head [x | x <- [1..], isDivisable x xs]


-- second attempt
gcd' m n = head [ x | x <- [m*n,m*n-1..1], m `mod` x == 0, n `mod` x == 0]
lcm' m n = m*n `div` (gcd' m n)

{-
*Main> foldl' lcm' 1 [20,19,18,17,16,15,14]
1627920
*Main> foldl' lcm' 1 [20,19..11]
^CInterrupted.
-}


-- binary euklid algorithm
egcd 0 n = n
egcd m 0 = m
egcd 1 n = 1
egcd m 1 = 1
egcd m n
  | m == n = m
  | even m && even n = 2 * egcd (m `div` 2) (n `div` 2)
  | even m && odd n = egcd (m `div` 2) n
  | odd m && even n = egcd m (n `div` 2)
  | odd m && odd n && n > m = egcd ((n-m) `div` 2) m
  | odd m && odd n && n < m = egcd ((m-n) `div` 2) n


elcm m n = (m*n) `div` (egcd m n)

{-
*Main> foldl' elcm 1 [20,19..1]
232792560
*Main> foldl' elcm 1 [1..20]
232792560
*Main> foldl' lcm 1 [1..20]
232792560
-}

-- haskell gcd
hgcd x 0  =  x
hgcd x y  =  hgcd y (x `rem` y)

hlcm m n = (m*n) `div` (hgcd m n)

