fun last' (xs : 'a list) : 'a = hd (rev xs)

fun last'' (xs : 'a list) : 'a = List.nth (xs, (length xs) - 1)

fun init' (xs : 'a list) : 'a list = List.take (xs, (length xs) - 1)

fun init'' (xs : 'a list) : 'a list = rev (List.drop (rev xs, 1))
