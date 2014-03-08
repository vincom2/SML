fun cross2 (l1, l2) =
    let
      fun all_pairs l b = List.map (fn x => (b,x)) l
      val l = List.map (all_pairs l2) l1
    in
      List.concat l
    end

fun cross3 (l1,l2,l3) =
    let
      val l' = cross2 (l2, l3)
      val l'' = cross2 (l1,l')
    in
      List.map (fn (a, (b,c)) => (a,b,c)) l''
    end

fun cartesian [] = []
  | cartesian [x] = List.map (fn e => [e]) x
  | cartesian (x::xs) =
    let
      val tail = cartesian xs
    in
      List.concat (List.map (fn e1 => List.map (fn e2 => e1::e2) tail) x)
    end
