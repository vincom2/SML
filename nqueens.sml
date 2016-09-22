(* note: ORD_SET and IntBinarySet are part of SMLNJ-lib, not the Standard Basis. *)

signature PROBLEM_SIZE = sig
  val n : int
end

signature NQUEENS = sig
  structure N : PROBLEM_SIZE

  exception NoMoreSolutions

  type queen = int * int (* transparent for convenience *)
  val n_queens : unit -> queen list
end

signature COLUMNS = sig
  structure N : PROBLEM_SIZE
  structure S : ORD_SET where type Key.ord_key = int

  type t = int list * S.set

  val full : t

  val next : t -> (int * t) option
  val next_row_without_col : int -> t -> t
end

signature PRETTY_PRINT = sig
  structure Q : NQUEENS

  val pretty_n_queens : unit -> unit
end

functor MkColumns (N : PROBLEM_SIZE) :> COLUMNS = struct
  structure N = N
  structure S = IntBinarySet

  type t = int list * S.set

  val full = let
    val l = List.tabulate (N.n, fn x => x)
    val s = S.addList (S.empty, l)
  in
    (l, s)
  end

  fun next ([], _) = NONE
    | next (c::cs, s) = SOME (c, (cs, s))

  fun next_row_without_col c (_, s) = let
    val s' = S.delete (s, c)
    val l = S.listItems s'
  in
    (l, s')
  end
end

functor NQueens (N : PROBLEM_SIZE) :> NQUEENS = struct
  structure N = N
  structure Columns = MkColumns(N)

  fun abs (n : int) = if n < 0 then ~n else n

  exception NoMoreSolutions

  type queen = int * int
  datatype cont = Cont of unit -> queen list * cont

  fun cannot_place (x1, y1) (x2, y2) =
    x1 = x2 orelse y1 = y2 orelse abs (x2 - x1) = abs (y2 - y1)

  fun can_place queen =
    List.foldl (fn (q, b) => not (cannot_place q queen) andalso b) true

  (* n_queens' : int -> row -> Columns.t -> queen list -> cont -> queen list * cont *)
  fun n_queens' 0 _ _ queens k = (queens, k)
    | n_queens' n r cols queens (Cont k) = if r = N.n then k () else
      (case Columns.next cols of
        NONE => k ()
      | SOME (c, cols') => if can_place (r, c) queens then
          n_queens' (n-1) (r+1) (Columns.next_row_without_col c cols') ((r,c)::queens)
            (Cont (fn () => n_queens' n r cols' queens (Cont k)))
          else n_queens' n r cols' queens (Cont k))

  val next : cont option ref = ref NONE

  fun n_queens () = case !next of
      NONE => let
        val (sol, k) = n_queens' N.n 0 Columns.full [] (Cont (fn () => raise NoMoreSolutions))
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

functor PrettyPrint (Q : NQUEENS) :> PRETTY_PRINT = struct
  structure Q = Q
  open Q

  val divider = (String.implode (List.tabulate (N.n*2+1 , fn _ => #"-"))) ^ "\n"
  fun print_divider () = TextIO.print divider

  fun print_row col =
    print ("|" ^ String.concatWith "|"
      (List.tabulate (N.n, fn c => if c = col then "o" else " ")) ^ "|\n")

  fun pretty_n_queens' n sol = if n = N.n then print_divider () else
  let
    val () = print_divider ()
    val ([(_, c)], rest) = List.partition (fn (r, _) => r = n) sol
    val () = print_row c
  in
    pretty_n_queens' (n+1) rest
  end

  fun pretty_n_queens () = pretty_n_queens' 0 (n_queens ())
end

