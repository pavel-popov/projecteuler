
-- take step in eratosphen cell by removing all multipliers of x in xs
ec_step :: Integer -> [Integer] -> [Integer]
ec_step 1 xs = xs
ec_step x xs = [ j | j <- xs, mod j x /= 0 ]


-- eratosphen cell recursive
ec_recc :: ([Integer], [Integer]) -> ([Integer], [Integer])
ec_recc (ps, []) = (ps, [])
ec_recc (ps, xs) = (ps++[x], ec_step x xs)
    where x = head xs


-- repeat :: f -> n -> repeat it
rep f (ps, []) = (ps, [])
rep f (ps, xs) = rep f (f (ps, xs))


-- primes till x
primes_till x = fst $ rep ec_recc ([], [2..x])

h x = (head x', length x')
  where x' = primes_till x

t n m = head . reverse $ take n (primes_till m)

{-
*Main> t 10001 200000
104743
-}
