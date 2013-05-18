functor AVLTree(K : ORDERED) : BTREE = struct
  structure Key = K
  type 'a entry = Key.t * 'a
  type height = int

  datatype 'a tree = Empty | Node of height * 'a tree * 'a entry * 'a tree

  val empty = Empty

  fun height Empty = 0
    | height (Node(h,_,_,_)) = h

  fun balFactor Empty = 0
    | balFactor (Node(_,l,_,r)) = (height l) - (height r)

  fun newNode (l,x,r) = Node(Int.max(height l, height r)+1,l,x,r)

  fun rotateLeft (Node(_,l1,x,Node(_,l2,y,r2))) = newNode(newNode(l1,x,l2),y,r2)
    | rotateLeft t = t

  fun rotateRight (Node(_,Node(_,l1,x,r1),y,r2)) = newNode(l1,x,newNode(r1,y,r2))
    | rotateRight t = t

  fun rebalance (t as Node(_,l,x,r)) =
      (case (balFactor t,balFactor l,balFactor r) of
       (2,~1,_) => rotateRight(newNode(rotateLeft l,x,r)) (* LR -> LL -> bal *)
     | (2,_,_) => rotateRight t (* LL -> bal *)
     | (~2,_,1) => rotateLeft(newNode(l,x,rotateRight r)) (* RL -> RR -> bal *)
     | _ => rotateLeft t) (* RR -> bal *)
    | rebalance _ = raise Fail "this should not happen!"

  fun insert (Empty,x) = newNode(Empty,x,Empty)
    | insert (Node(h,l,c as (k,v),r),x as (k',v')) =
      (case K.compare(k,k') of
       EQUAL => Node(h,l,(k,v'),r)
     | LESS => rebalance(newNode(insert(l,x),c,r))
     | GREATER => rebalance(newNode(l,c,insert(r,x))))

  fun find (Empty,_) = NONE
    | find (Node(_,l,(k,v),r),k') =
      (case K.compare(k,k') of
       EQUAL => SOME v
     | LESS => find(l,k')
     | GREATER => find(r,k'))
end
