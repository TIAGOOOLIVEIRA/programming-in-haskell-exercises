(* SML doesn't have list comprehensions so we use normal functions *)

val generateIntsUpTo = fn n =>
  let val rec helper = fn current => fn xs =>
    if current <= n
    then helper (current + 1) (current :: xs)
    else xs
  in
    rev (helper 1 [])
  end

val pyths = fn n =>
  let
    val numbers = generateIntsUpTo n
  in
    foldr (fn (x, acc) =>
      foldr (fn (y, acc) =>
        foldr (fn (z, acc) =>
          if x * x + y * y = z * z
          then (x, y, z) :: acc
          else acc
        ) acc numbers
      ) acc numbers
    ) [] numbers
  end

val perfects = fn n =>
  let
    val factors = fn n =>
      foldr (fn (x, acc) =>
        if x <> n andalso n mod x = 0
        then x :: acc
        else acc
      ) [] (generateIntsUpTo n)
  in
    foldr (fn (x, acc) =>
      if x = foldr (fn (x, acc) => acc + x) 0 (factors x)
      then x :: acc
      else acc
    ) [] (generateIntsUpTo n)
  end

val scalarProduct = fn xs => fn ys =>
  foldl (fn (n, sum) => sum + n) 0 (foldl
    (fn (n, ns) => #1 n * #2 n :: ns) [] (ListPair.zip (xs, ys)))
