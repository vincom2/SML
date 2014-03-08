fun cross2 (l1, l2) =
    let
      fun all_pairs l b = List.map (fn x => (b,x)) l
      val l = List.map (all_pairs l2) l1
    in
      List.concat l
    end

val words = ["ha", "heh", "hmm"]
val concat_words = map (fn w1 => fn w2 => w1 ^ w2) words
val punctuation = [".", "!", "?"]
val res = map (fn (f,s) => f s) (cross2 (concat_words, punctuation))
