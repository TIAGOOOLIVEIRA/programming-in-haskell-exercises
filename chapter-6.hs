{-# LANGUAGE UnicodeSyntax #-}

-- "Decide if all logical values in a list are true"

and' ∷ [Bool] → Bool
and' [] = True
and' (False:_) = False
and' (_:xs) = and' xs

-- "Concatenate a list of lists"

concat' ∷ [[a]] → [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- "Produce a list with n identical elements"

replicate' ∷ Int → a → [a]
replicate' 0 x = []
replicate' n x = x : replicate' (n - 1) x

-- "Select the nth element of a list"

(!!!) ∷ [a] → Int → a
(x:_) !!! 0 = x
(_:xs) !!! n = xs !!! (n - 1)

-- "Decide if a value is an element of a list"

elem' ∷ Eq a ⇒ a → [a] → Bool
elem' y [] = False
elem' y (x:xs) = if y == x then True else elem' y xs

-- "Define a recursive function merge ∷ Orda ⇒ [a] → [a] → [a] that merges two sorted lists of values to give a single sorted list"

merge ∷ Ord a ⇒ [a] → [a] → [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) =
	if x < y
	then x : merge xs (y : ys)
	else y : merge (x : xs) ys

-- "Define a recursive function msort ∷ Orda ⇒ [a] → [a] that implements merge sort, which can be specified by the following two rules:
-- Lists of length ≤ 1 are already sorted;
-- Other lists can be sorted by sorting the two halves and merging the resulting lists"

msort ∷ Ord a ⇒ [a] → [a]
msort [] = []
msort (x : []) = [x]
msort xs = merge (msort (take divideAt xs)) (msort (drop divideAt xs)) where
	divideAt = div (length xs) 2
