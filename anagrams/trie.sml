structure WordDict : TRIE = struct
  type myword = string
  datatype 'a dict = Trie of ('a dict * 'a option) Vector.vector | Empty

  (* assumes ASCII *)
  fun char_pos c = let val c' = Char.toLower c in (Char.ord c') - 97 end

  (*val empty = Trie(Vector.tabulate(26, fn n => (false,Empty)))*)
  val empty = Empty

  fun lookup' (Empty, _) = NONE
    | lookup' (_, []) = NONE
    | lookup' (Trie d, [c]) =
       let val (_,ret) = Vector.sub(d, char_pos c) in ret end
    | lookup' (Trie d, c::cs) =
       let val (d2,_) = Vector.sub(d, char_pos c) in
       lookup' (d2,cs) end
  fun lookup (d,w) = lookup'(d, String.explode w)
  
  fun insert' (d, [], _) = d
    | insert' (Empty, [c], a) = let val m = char_pos c in
      Trie(Vector.tabulate(26, fn n => if n = m then (Empty,SOME a) else (Empty,NONE))) end
    | insert' (Trie d, [c], a) = let val m = char_pos c in
      Trie(Vector.tabulate(26, fn n =>
        if n = m then let val (d2,_) = Vector.sub(d, m) in
        (d2,SOME a) end
        else Vector.sub(d, n))) end
    | insert' (Empty, c::cs, a) = let val m = char_pos c in
      Trie(Vector.tabulate(26, fn n => if n = m then (insert'(Empty,cs,a),NONE) else (Empty,NONE))) end
    | insert' (Trie d, c::cs, a) = let val m = char_pos c in
      Trie(Vector.tabulate(26, fn n =>
        if n = m then let val (d2,a') = Vector.sub(d,m) in
        (insert'(d2,cs,a),a') end
        else Vector.sub(d,n))) end
  fun insert (d,w,a) = insert' (d, String.explode w, a)
end
