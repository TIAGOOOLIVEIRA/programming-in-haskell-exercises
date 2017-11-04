val rec and' = fn
    [] => true
  | false::_ => false
  | _::xs => and' xs

val rec concat = fn
    [] => []
  | x::xs => x @ concat xs

fun replicate 0 x = []
  | replicate n x = replicate (n - 1) x

infix !!
fun (x::_) !! 0 = x
  | (_::xs) !! n = xs !! (n - 1)

fun elem y [] = false
  | elem y (x::xs) = if y = x then true else elem y xs

fun merge xs ys =
  let
    fun helper xs ys merged =
      case (xs, ys) of
        ([], []) => merged
      | ([], ys) => merged @ ys
      | (xs, []) => merged @ xs
      | (x :: xs, y :: ys) =>
        if x < y
        then helper xs (y :: ys) (merged @ [x])
        else helper (x :: xs) ys (merged @ [y])
  in
    helper xs ys []
  end

fun msort xs =
  case xs of
    [] => []
  | x :: [] => [x]
  | xs =>
    let
      val divideAt = length xs div 2
    in
      merge (msort (List.take (xs, divideAt))) (msort (List.drop (xs, divideAt)))
    end
