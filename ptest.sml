Control.Print.printLength := 1000;

fun length [] = 0
  | length (x::xs) = 1 + (length xs)

fun removeOne [] = raise Fail "can't remove from empty list!"
  | removeOne [x] = [(x, [])]
  | removeOne (x::xs) =
      (x,xs) :: (map (fn (y,L) => (y,x::L)) (removeOne xs))

fun permHelper [] = []
  | permHelper ((currh, currL)::xs) =
      (map (fn A => currh::A) (permutations currL)) @
      (permHelper xs)
and permutations [] = []
  | permutations [x] = [[x]]
  | permutations L = permHelper (removeOne L)

val 720 = length (permutations [1,2,3,4,5,6])