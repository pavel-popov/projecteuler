-- take step in eratosphen cell by removing all multipliers of x in xs
ec_step :: Integer -> [Integer] -> [Integer]
ec_step 1 xs = xs
ec_step x xs = [ j | j <- xs, mod j x /= 0 ]


-- eratosphen cell recursive
ec_recc :: ([Integer], [Integer]) -> ([Integer], [Integer])
ec_recc (ps, []) = (ps, [])
ec_recc (ps, xs) = (x:ps, ec_step x xs)
    where x = head xs


-- repeat :: f -> n -> repeat it
rep f (ps, []) = (ps, [])
rep f (ps, xs) = rep f (f (ps, xs))


-- primes till x
primes_till x = fst $ rep ec_recc ([], [2..x])


-- checks if x is prime
isPrime :: Integer -> Bool
isPrime x = if mod x 2 == 0
             then False
             else [ f | f <- [1..x], x `mod` f == 0 ] == [1,x]


-- prime factors of x
prime_factors :: Integer -> [Integer]
prime_factors x = [ f | f <- primes_till x, mod x f == 0]


-- largest factor of given number
largest_factor :: Integer -> Integer
largest_factor = maximum . prime_factors


-- DICSON METHOD


vector_base :: Integer -> [Integer]
vector_base = primes_till . m
    where m x = truncate $ sqrt $ exp $ sqrt $ log f * log (log f)
            where f = fromIntegral x


dicson_factors :: Integer -> [Integer]
dicson_factors x = [ f | f <- vector_base x, x `mod` f == 0]

concat' :: [[a]] -> [a]
concat' xss = foldr (++) [] xss

-- FERMA METHOD - worked

isSquare :: (Integral a) => a -> Bool
isSquare n = (round . sqrt $ fromIntegral n) ^ 2 == n


sqrt' :: (Integral a) => a -> a
sqrt' x = truncate . sqrt $ fromIntegral x


ferma' :: (Integral a) => a -> a -> [a]
ferma' n k = if isSquare y
            then [x + sqrt' y, x - sqrt' y]
            else ferma' n (k+1)
    where y = x^2 - n
          x = (sqrt' n) + 1 + k


full_ferma x = [ if isPrime j then [j] else concat' (full_ferma j) | j <- ferma' x 1]

ferma = concat' . full_ferma

-- *Main> ferma 600851475143
-- [1471,839,6857,71]


