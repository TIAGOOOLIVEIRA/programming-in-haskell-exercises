{-# LANGUAGE UnicodeSyntax #-}

-- "A triple (x,y,z) of positive integers is called pythagorean if x2 + y2 = z2. Using a list comprehension, define a function pyths ∷ Int → [(Int,Int,Int)] that maps an integer n to all such triples with components in [1..n]"

pyths ∷ Int → [(Int, Int, Int)]
pyths n = [(x, y, z) | x ← [1..n], y ← [1..n], z ← [1..n], x^2 + y^2 == z^2]

-- "A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself. Using a list comprehension, define a function perfects ∷ Int → [Int] that returns the list of all perfect numbers up to a given limit"

factors ∷ Int → [Int]
factors n = [x | x ← [1..n - 1], n `mod` x == 0]

perfects ∷ Int → [Int]
perfects n = [x | x ← [1..n], x == sum (factors x)]

-- "The scalar product of two lists of integers xs and ys of length n is give by the sum of the products of the corresponding integers. Using a list comprehension, define a function that returns the scalar product of two lists"

scalar_product ∷ [Int] → [Int] → Int
scalar_product xs ys = sum [x * y | (x, y) ← zip xs ys]
