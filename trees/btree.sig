signature ORDERED = sig
  type t
  val compare : t * t -> order
end

signature BTREE = sig
  structure Key : ORDERED
  type 'a entry = Key.t * 'a

  type 'a tree

  val empty : 'a tree
  val find : 'a tree * Key.t -> 'a option
  val insert : 'a tree * 'a entry -> 'a tree
end