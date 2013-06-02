functor AnagramDict (Dict : TRIE where type myword = string) : ANAGRAMDICT = struct
  structure D = Dict
  type wordlist = D.myword list

  val empty = D.empty

  (* this is going to be stupidly inefficient since it first has to look up
   * the current list of anagrams stored at that position in the trie
   * and *then* do an insertion. designing stuff to be efficient is
   * really hard ): *)
  (* also it, uh, sorts the word. how much worse can this get? *)
  (* also, it assumes no duplicate insertions because whatever *)
  fun find (d, w) =
      let
        val sw = String.implode (ListMergeSort.sort Char.>= (String.explode w))
        (*val _ = print ("looking up " ^ sw ^ "\n")*)
      in
        D.lookup(d,sw)
      end

  fun add (d, w) =
      let
        val sw = String.implode (ListMergeSort.sort Char.>= (String.explode w))
        (*val _ = print ("adding to " ^ sw ^ "\n")*)
      in
        case find(d,sw) of
        NONE => D.insert(d, sw, [w])
      | SOME L => D.insert(d, sw, w::L)
      end
end
