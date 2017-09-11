{-# LANGUAGE UnicodeSyntax #-}

-- "Show how the library function last that selects the last element of a list can be defined using the funcions introduce in this lecture"

-- The mentioned functions are head, tail, !!, take, drop, length, sum, product, ++, reverse and div

last' ∷ [a] → a
last' xs = head (reverse xs)

last'' ∷ [a] → a
last'' xs = xs !! (length xs - 1)

-- "Similarly, show how the library function init that removes the last element from a list can be defined in two different ways"

init' ∷ [a] → [a]
init' xs = take (length xs - 1) xs

init'' ∷ [a] → [a]
init'' xs = reverse (drop 1 (reverse xs))
