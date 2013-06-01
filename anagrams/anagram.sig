signature ANAGRAM = sig
  structure D : TRIE where type myword = string

  type wordlist = D.myword list

  val empty : wordlist D.dict
  val find : wordlist D.dict * D.myword -> wordlist option
  val add : wordlist D.dict * D.myword -> wordlist D.dict
end
