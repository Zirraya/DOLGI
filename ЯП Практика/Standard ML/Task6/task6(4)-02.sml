(* ВАРИАНТ №2 *)
fun f6 [] = NONE
  | f6 (x :: xs) =
      let
        fun helper [] min max = (min, max)
          | helper (x :: xs) min max =
                helper xs (if x < min then x + 0.0 else min)
                          (if x > max then x else max)
      in
        SOME (helper xs x x)
      end

val test02 = f6 []
val test12 = f6 [12.0]
val test22 = f6 [1.0, 2.0, 2.0, 2.0, 3.0, 3.0, 4.0, 5.0, 7.9]
val test32 = f6 [115.5, 111.7, 111.0, 0.5, 2.0, 2.0, 3.0, 3.0, 3.0,
                 2.0, 2.0, 1.0, 2.0, 3.0, 3.0, 43.0, 4.0, 5.0,
                 6.0, 6.0, 4.0, 7.0]
val test42 = f6 [~100.5, 3.2, 0.0, ~2.1, 10.7]
val test52 = f6 [~5.4, 500.0, 55.5, ~5.4]
val test62 = f6 []