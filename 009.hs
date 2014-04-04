

squares = [ j^2 | j <- [1..1000], j^2 <= 1000 ]
triplets = [ (a, b, c) | a <- squares, b <- squares, c <- squares, a < b, b < c, a+b+c==1000 ]

sqrt' = truncate . sqrt

check = map (\(x, y, z) -> (sqrt' x, sqrt' y, sqrt' z)) triplets
check2 = map (\(x, y, z) -> x^2+y^2==z^2) check


-- not optimal solution
--triplet = head [ (a, b, c) | a <- [1..1000], b <- [1..1000], c <- [1..1000], a < b, b < c, a^2+b^2==c^2, a+b+c==1000]

-- analytical solution
{-
 - a^2+b^2==c^2 -> a^2 == c^2 - b^2 == (c-b)(c+b)
 - a+b+c == 1000 -> c+b == 1000-a
 - a+b+c == 1000 -> c-b == 1000-a-2b
 - a^2 = (1000-a-2b)(1000-a)
-}

(a, b) = head [ (a, b) | a <- [1..1000], b <- [1..1000], a < b, a^2==(1000-a-2*b)*(1000-a) ]

c = truncate $ sqrt $ fromIntegral (a^2+b^2)

{-
*Main> a
200
*Main> b
375
*Main> c
425
*Main> a+b+c
1000
*Main> a*b*c
31875000
-}


