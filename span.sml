fun span' (_ : 'a -> bool) [] l1 = (List.rev l1,[])
  | span' p (l2 as x::xs) l1 = if not (p x) then (List.rev l1,l2)
                              else span' p xs (x::l1)

fun span p l = span' p l []

fun initials' [] = []
  | initials' s = let
      val (i::_, rest) = span (fn c => c <> #" ") s
      val i' = case rest of
               [] => []
             | _::s' => initials' s'
    in
      [i,#".",#" "] @ i'
    end
fun initials s = String.implode (initials' (String.explode s))