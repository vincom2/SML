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
