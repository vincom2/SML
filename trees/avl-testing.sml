structure IntLt : ORDERED = struct
  type t = int
  val compare = Int.compare
end

structure StringLt : ORDERED = struct
  type t = string
  val compare = String.compare
end

structure A = AVLTree(StringLt)