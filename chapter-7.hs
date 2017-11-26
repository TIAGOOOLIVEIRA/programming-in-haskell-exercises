{-# LANGUAGE UnicodeSyntax #-}

-- "Express the comprehension [f x | x ← xs, p x] using the functions map and filter."

comprehension xs f p = map f $ filter p xs

-- "Redefine map f and filter p using foldr."

map' f = foldr (\x acc → f x : acc) []

filter' p = foldr (\x acc → if p x then x : acc else acc) []
