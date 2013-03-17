fun powerset [] = []
  | powerset [x] = [[],[x]]
  | powerset (x::xs) =
    let
      val power_subset = powerset xs
    in
      (List.map (fn L => x::L) power_subset) @ power_subset
    end

(* no map allowed! *)
fun union (e, [] : ''a list) : ''a list = [e]
  | union (e, x::xs) =
    if e = x then x::xs
    else x::union(e, xs)
fun insert (e : ''a, [] : ''a list list) : ''a list list = []
  | insert (e, s::ss) = union(e, s)::insert(e, ss)
fun pset [] = []
  | pset [x] = [[],[x]]
  | pset (x::xs) =
    let
      val power_subset = powerset xs
    in
      power_subset @ insert(x, power_subset)
    end