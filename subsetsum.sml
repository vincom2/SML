fun subset_sum ([] : int list, s : int) : bool * int list =
      if s = 0 then (true, [])
      else (false, [])
  | subset_sum (_ : int list, 0 : int) : bool * int list = (true, [])
  | subset_sum (x::xs : int list, s : int) : bool * int list =
      let val ((w,wList),(wo,woList)) =
      (subset_sum(xs, s-x), subset_sum(xs, s)) in
        case (w,wo) of
          (true,_) => (true, x::wList)
        | (_,true) => (true, woList)
        | (_,_) => (false, [])
      end