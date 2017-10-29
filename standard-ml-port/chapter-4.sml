fun safetailA (xs : 'a list) : 'a list = if null xs then [] else tl xs

val safetailB = fn
    [] => []
  | _::xs => xs

fun safetailC [] = []
  | safetailC (_::xs) = xs

infix ||
fun false || false = false
  | _ || _ = true

infix &&
fun a && b = if a = true andalso b = true then true else false

infix &&&
fun a &&& b = if a = false then false else b
