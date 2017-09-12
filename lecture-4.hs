{-# LANGUAGE UnicodeSyntax #-}

-- "Consider a function safetail that behaves in the same way as tail, except that safetail maps the empty list to the empty list, whereas tail gives an error in this case. Define safetail using:
-- (a) a conditional expression;
-- (b) guarded equations;
-- (c) patternmatching"

safetailA ∷ [a] → [a]
safetailA xs = if null xs then [] else tail xs

safetailB ∷ [a] → [a]
safetailB xs
	| null xs = []
	| otherwise = tail xs

safetailC ∷ [a] → [a]
safetailC [] = []
safetailC (_:xs) = xs

-- "Give three possible definitions for the logical or operator (||) using pattern matching"

-- As far as I know the other two possible definitions are variations of this one with explicit True, so meh:

(|||) ∷ Bool → Bool → Bool
False ||| False = False
_ ||| _ = True

(||||) ∷ Bool → Bool → Bool
(||||) False False = False
(||||) _ _ = True

-- "Redefine the following version of (&&) using conditionals rather than patterns
-- True && True = True
-- _ &&_ =False"

(&&&) ∷ Bool → Bool → Bool
(&&&) a b = if a == True && b == True then True else False

-- "Do the same for the following version:
-- True && b = b
-- False && _ = False"

(&&&&) ∷ Bool → Bool → Bool
(&&&&) a b = if a == False then False else b
