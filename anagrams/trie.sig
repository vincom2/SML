signature TRIE = sig
  type myword
  type 'a dict

  val empty : 'a dict
  val lookup : 'a dict * myword -> 'a option
  val insert : 'a dict * myword * 'a -> 'a dict
  (*val delete : 'a dict * myword -> 'a dict*) (*deletion is hard yo *)
end
