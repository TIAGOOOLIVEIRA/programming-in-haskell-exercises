fun comprehension xs f p = map f (List.filter p xs)

fun map' f = foldr (fn (x, acc) => f x :: acc) []

fun filter' p = foldr (fn (x, acc) => if p x then x :: acc else acc) []
