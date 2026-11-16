fun f7 n =
  let
    fun prod j stop ri acc =
      if j > stop then acc
      else
        let
          val rj = real j
        in
          prod (j + 1) stop ri (acc * (ri - rj * rj * rj))
        end

    fun sum i stop innerStop acc =
      if i > stop then acc
      else
        sum (i + 1) stop innerStop
          (acc + prod 2 innerStop (real i) 1.0)
  in
    sum 1 (n + 2) (n - 2) 0.0
  end

val test1 = f7 0
val test2 = f7 1
val test3 = f7 55
val test4 = f7 8
val test5 = f7 72
val test6 = f7 ~18
val test7 = f7 ~211