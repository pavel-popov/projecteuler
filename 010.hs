
ec_step _ [] = []
ec_step x xs = [ y | y <- xs, y `mod` x /= 0 ]

ec_recc :: ([Integer], [Integer]) -> ([Integer], [Integer])
ec_recc (ps, []) = (ps, [])
ec_recc (ps, xs) = ec_recc (x:ps, ec_step x xs)
    where x = head xs



-- primes till x
primes_till x = fst $ ec_recc ([], [2..x])

sum_below_2m = sum $ primes_till 2000000


-- from wiki -- working quick example
primesTo n = eratos [2..n]  where
   eratos []     = []
   eratos (p:xs) = p : eratos (xs `minus` [p*p, p*p+p..n])

minus (x:xs) (y:ys) = case (compare x y) of
   LT -> x : minus  xs (y:ys)
   EQ ->     minus  xs    ys
   GT ->     minus (x:xs) ys
minus  xs     _     = xs


sum_below_2m_e = sum $ primesTo 2000000
{-
  142913828922
-}
