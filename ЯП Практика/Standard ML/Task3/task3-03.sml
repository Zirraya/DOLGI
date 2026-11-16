
fun f3 (a1 : real * real * int list, a2 : (int * int) list)
    : real * real * real * real list =
  let
    val x = #2 a1
    val xPlus5 = x + 5.0
    val xMinus3 = x - 3.0
    val xPlusY = x + real (#1 (hd (tl a2)))
    val sumSquared = xPlusY * xPlusY
    val firstValue = sumSquared + xMinus3 / xPlus5
    val thirdValue = sumSquared / xMinus3
  in
    ( firstValue, xPlus5, thirdValue
    , [firstValue, thirdValue] )
  end

val test0 = f3 ((1.0, 4.0, [6, 0, 3]), [(3, 7), (2, 8)])
val test1 = f3 ((2.0, 6.0, [1, 2, 3]), [(9, 4), (5, 6)])
val test2 = f3 ((0.0, 8.5, [4, 5, 6]), [(1, 2), (7, 3)])





