signature POWER_SERIES = sig
  type powerseries
  val series : real list -> powerseries
  val coeffs : powerseries -> real list
  val coeff : powerseries * int -> real
  val toString : powerseries -> string
  val isEq : powerseries * powerseries -> bool
  val add : powerseries * powerseries -> powerseries
  val mul : powerseries * powerseries -> powerseries
  val negate : powerseries -> powerseries
  val abs : powerseries -> powerseries
  val sign : powerseries -> powerseries
  val sub : powerseries * powerseries -> powerseries
  val fromReal : real -> powerseries
  val isGT : powerseries * powerseries -> bool
  val isLT : powerseries * powerseries -> bool
  val divide : powerseries * powerseries -> powerseries
  val recip : powerseries -> powerseries
end

structure PowerSeries : POWER_SERIES = struct
  datatype powerseries = series of real list

  val termCount = 10

  fun coeffs (series xs) = xs

  fun coeffAt ([], _) = 0.0
    | coeffAt (x :: xs, n) =
        if n < 0 then 0.0
        else if n = 0 then x
        else coeffAt (xs, n - 1)

  fun coeff (s, n) = coeffAt (coeffs s, n)

  fun dropZeros [] = []
    | dropZeros (x :: xs) =
        if Real.== (x, 0.0) then dropZeros xs else x :: xs

  fun sNormalize (series xs) = series (List.rev (dropZeros (List.rev xs)))

  fun sMap f (series xs) = series (List.map f xs)

  fun addLists ([], ys) = ys
    | addLists (xs, []) = xs
    | addLists ((x : real) :: xs, y :: ys) = x + y :: addLists (xs, ys)

  fun scale (_, []) = []
    | scale (x : real, y :: ys) = x * y :: scale (x, ys)

  fun sConvolve (_, []) = []
    | sConvolve ([], _) = []
    | sConvolve (x :: xs, ys) =
        addLists (scale (x, ys), 0.0 :: sConvolve (xs, ys))

  fun indexed (_, []) = []
    | indexed (k, x :: xs) = (x, k) :: indexed (k + 1, xs)

  fun isNonZero (c, _) = not (Real.== (c, 0.0))

  fun term (c, 0) = Real.toString c
    | term (c, 1) = Real.toString c ^ "x"
    | term (c, k) = Real.toString c ^ "x^" ^ Int.toString k

  fun showRest [] = ""
    | showRest ((c, k) :: ts) =
        (if c < 0.0 then " - " else " + ")
        ^ term (Real.abs c, k)
        ^ showRest ts

  fun showTerms [] = "0.0"
    | showTerms ((c, k) :: ts) =
        (if c < 0.0 then "-" else "") ^ term (Real.abs c, k) ^ showRest ts

  fun toString s =
    showTerms (List.filter isNonZero (indexed (0, coeffs (sNormalize s))))

  fun eqLists ([], []) = true
    | eqLists (x :: xs, y :: ys) = Real.== (x, y) andalso eqLists (xs, ys)
    | eqLists _ = false

  fun isEq (a, b) = eqLists (coeffs (sNormalize a), coeffs (sNormalize b))

  fun add (a, b) = series (addLists (coeffs a, coeffs b))

  fun mul (a, b) = series (sConvolve (coeffs a, coeffs b))

  fun negate s = sMap Real.~ s

  fun abs s = sMap Real.abs s

  fun sign s = sMap (real o Real.sign) s

  fun sub (a, b) = add (a, negate b)

  fun fromReal x = series [x]

  fun lexLT ([], []) = false
    | lexLT (x :: xs, y :: ys) =
        if Real.== (x, y) then lexLT (xs, ys) else x < y
    | lexLT _ = false

  fun isLT (a, b) =
    let
      val xs = coeffs (sNormalize a)
      val ys = coeffs (sNormalize b)
      val nx = List.length xs
      val ny = List.length ys
    in
      nx < ny orelse nx = ny andalso lexLT (List.rev xs, List.rev ys)
    end

  fun isGT (a, b) = isLT (b, a)

  fun dotProd ([], _) = 0.0
    | dotProd (_, []) = 0.0
    | dotProd (b :: bs, c :: cs) = b * c + dotProd (bs, cs)

  fun quotient (xs, b0, bt, k, acc) =
    if k >= termCount then List.rev acc
    else
      let
        val c = (coeffAt (xs, k) - dotProd (bt, acc)) / b0
      in
        quotient (xs, b0, bt, k + 1, c :: acc)
      end

  fun divCoeffs (_, []) = raise Div
    | divCoeffs (xs, b0 :: bt) =
        if Real.== (b0, 0.0) then raise Div
        else quotient (xs, b0, bt, 0, [])

  fun divide (a, b) = series (divCoeffs (coeffs a, coeffs b))

  fun recip s = divide (fromReal 1.0, s)
end

val s1 = PowerSeries.series [1.0, 2.0, 3.0]
val s2 = PowerSeries.series [0.0, 1.0]
val s3 = PowerSeries.series [1.0, ~1.0, 0.0, 0.0]
val s4 = PowerSeries.series [1.0, ~1.0]
val s5 = PowerSeries.series [~2.0, 0.0, 4.0]
val zeroSeries = PowerSeries.series []

val s1Coeffs = PowerSeries.coeffs s1
val s1Coeff2 = PowerSeries.coeff (s1, 2)
val s1Coeff7 = PowerSeries.coeff (s1, 7)
val s3Coeffs = PowerSeries.coeffs s3

val s1String = PowerSeries.toString s1
val s2String = PowerSeries.toString s2
val s3String = PowerSeries.toString s3
val s5String = PowerSeries.toString s5
val zeroString = PowerSeries.toString zeroSeries

val s3EqS4 = PowerSeries.isEq (s3, s4)
val s1EqS4 = PowerSeries.isEq (s1, s4)

val s1PlusS2 = PowerSeries.toString (PowerSeries.add (s1, s2))
val s1PlusS5 = PowerSeries.toString (PowerSeries.add (s1, s5))
val s1MulS2 = PowerSeries.toString (PowerSeries.mul (s1, s2))
val s1MulS4 = PowerSeries.toString (PowerSeries.mul (s1, s4))

val s5Negate = PowerSeries.toString (PowerSeries.negate s5)
val s5Abs = PowerSeries.toString (PowerSeries.abs s5)
val s5Sign = PowerSeries.toString (PowerSeries.sign s5)
val s1MinusS1 = PowerSeries.toString (PowerSeries.sub (s1, s1))
val s1MinusS5 = PowerSeries.toString (PowerSeries.sub (s1, s5))

val fromReal3 = PowerSeries.toString (PowerSeries.fromReal 3.0)

val s1GTS2 = PowerSeries.isGT (s1, s2)
val s1LTS2 = PowerSeries.isLT (s1, s2)
val s1LTS5 = PowerSeries.isLT (s1, s5)
val s5LTS1 = PowerSeries.isLT (s5, s1)
val s1GTS1 = PowerSeries.isGT (s1, s1)

val s1DivS4 = PowerSeries.toString (PowerSeries.divide (s1, s4))
val oneDivS4 =
  PowerSeries.toString (PowerSeries.divide (PowerSeries.fromReal 1.0, s4))
val s4Recip = PowerSeries.toString (PowerSeries.recip s4)
val s4Recip2 = PowerSeries.toString (PowerSeries.recip (PowerSeries.recip s4))