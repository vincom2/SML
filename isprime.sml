fun isPrime n =
      if n<2 orelse n mod 2 = 0 then false else
      let
        fun testFrom m =
              if m*m > n then true
              else if n mod m = 0 then false
              else testFrom (m+2)
      in testFrom 3
      end