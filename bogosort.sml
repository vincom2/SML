use "ptest.sml";

fun isSorted [] = true
  | isSorted [x] = true
  | isSorted (x::y::L) = x <= y andalso isSorted (y::L)

fun bogoHelper [] = []
  | bogoHelper (x::xs) = if isSorted x then x
      else bogoHelper xs

fun bogosort L = let val P = permutations L in bogoHelper P end