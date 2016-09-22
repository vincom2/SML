signature NQUEENS = sig
  type queen = int * int (* transparent for convenience *)
  val n_queens : unit -> queen list
end

functor NQueens (structure N : sig val n : int end) :> NQUEENS = struct
  fun abs (n : int) = if n < 0 then ~n else n

  type queen = int * int
  datatype cont = Cont of unit -> queen list * cont

  fun cannot_place (x1, y1) (x2, y2) =
    x1 = x2 orelse y1 = y2 orelse abs (x2 - x1) = abs (y2 - y1)

  fun can_place (queen : queen) =
    List.foldl (fn (q, b) => not (cannot_place q queen) andalso b) true

  (* column major *)
  fun next_pos (x, y) =
    if y+1 >= N.n then
      if x+1 >= N.n then NONE else SOME (x+1, 0)
      else SOME (x, y+1)

  (* n_queens' : int -> queen option -> queen list -> cont -> queen list * cont *)
  fun n_queens' (0 : int) (_ : queen option) (queens : queen list) (k : cont) : queen list * cont = (queens, k)
    | n_queens' _ NONE _ (Cont k) = k ()
    | n_queens' n (SOME queen) queens k =
    if can_place queen queens then n_queens' (n-1) (next_pos queen) (queen::queens) (Cont (fn () => n_queens' n (next_pos queen) queens k))
    else n_queens' n (next_pos queen) queens k

  val next : cont option ref = ref NONE

  fun n_queens () = case !next of
      NONE => let
        val (sol, k) = n_queens' N.n (SOME (0,0)) [] (Cont (fn () => raise Fail "no more solutions"))
        val () = next := SOME k
      in
        sol
      end
    | SOME (Cont k) => let
        val (sol, k') = k ()
        val () = next := SOME k'
      in
        sol
      end
end
