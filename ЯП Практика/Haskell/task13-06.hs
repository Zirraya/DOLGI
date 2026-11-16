data PowerSeries a = Series [a]

coeffs (Series xs) = xs

coeffAt [] _ = 0
coeffAt (x : xs) k = if k == 0 then x else coeffAt xs (k - 1)

coeff (Series xs) k = coeffAt xs k

trimZeros [] = []
trimZeros (x : xs) = if x == 0 && rest == [] then [] else x : rest
  where rest = trimZeros xs

sNormalize (Series xs) = Series (trimZeros xs)

sMap f (Series xs) = Series (map f xs)

addLists [] ys = ys
addLists xs [] = xs
addLists (x : xs) (y : ys) = x + y : addLists xs ys

sConvolve [] _ = []
sConvolve _ [] = []
sConvolve (x : xs) ys = addLists (map (x *) ys) (0 : sConvolve xs ys)

cmpLists [] [] = EQ
cmpLists [] _ = LT
cmpLists _ [] = GT
cmpLists (x : xs) (y : ys) = if rest == EQ then compare x y else rest
  where rest = cmpLists xs ys

showTerm 0 c = show c
showTerm 1 c = show c ++ "x"
showTerm k c = show c ++ "x^" ++ show k

showTerms _ [] = []
showTerms k (c : cs) = if c == 0 then rest else showTerm k c : rest
  where rest = showTerms (k + 1) cs

joinTerms [] = ""
joinTerms [t] = t
joinTerms (t : ts) = t ++ " + " ++ joinTerms ts

negDivide b0 t = negate t / b0

instance (Show a, Num a, Eq a) => Show (PowerSeries a) where
  show s = if terms == [] then show (coeff s 0) else joinTerms terms
    where terms = showTerms 0 (coeffs s)

instance (Eq a, Num a) => Eq (PowerSeries a) where
  (==) x y = coeffs (sNormalize x) == coeffs (sNormalize y)

instance (Num a, Eq a) => Num (PowerSeries a) where
  (+) (Series xs) (Series ys) = Series (addLists xs ys)
  (*) (Series xs) (Series ys) = Series (sConvolve xs ys)
  negate s = sMap negate s
  abs s = sMap abs s
  signum s = sMap signum s
  fromInteger n = Series [fromInteger n]

instance (Num a, Eq a, Ord a) => Ord (PowerSeries a) where
  compare x y = cmpLists (coeffs (sNormalize x)) (coeffs (sNormalize y))

instance (Fractional a, Eq a) => Fractional (PowerSeries a) where
  recip (Series (b0 : bt)) = Series (take terms cs)
    where terms = 10
          cs = recip b0 : map (negDivide b0) (sConvolve bt cs)
  (/) x y = Series (take terms (coeffs (x * recip y)))
    where terms = 10
  fromRational r = Series [fromRational r]

-------------------------------------------------------------------------------

main = do
  let s1 = Series [1.0, 2.0, 3.0]
  let s2 = Series [0.0, 1.0]
  let s3 = Series [1.0, -1.0, 0.0, 0.0]
  let s4 = Series [1.0, -1.0]
  let s5 = Series [-2.0, 0.0, 4.0]
  let n1 = Series [Series [1.0, 2.0], Series [3.0]]
  let n2 = Series [Series [0.0, 1.0], Series [1.0, 1.0]]
  print $ s1
  print $ s2
  print $ s3
  print $ s5
  print $ Series [0.0, 0.0]
  print $ Series []
  print $ Series [1, 2, 3]
  print $ coeffs s1
  print $ coeffs s3
  print $ coeff s1 2
  print $ coeff s1 7
  print $ sNormalize s3
  print $ coeffs (sNormalize s3)
  print $ sMap (* 2) s1
  print $ sMap abs s5
  print $ sConvolve [1, 2, 3] [4, 5]
  print $ sConvolve [] [1.0, 2.0]
  print $ sConvolve [1.0, 2.0] []
  print $ s3 == s4
  print $ s1 == s4
  print $ s1 /= s5
  print $ s1 + s2
  print $ s1 + s5
  print $ s1 * s2
  print $ s1 * s4
  print $ Series [1, 2, 3] * Series [4, 5]
  print $ negate s5
  print $ abs s5
  print $ signum s5
  print $ s1 - s1
  print $ s1 - s5
  print $ s1 + 1
  print $ 2 * s1
  print $ s5 - 1
  print $ s1 * 0.5
  print $ s1 / 2
  print $ 1 / s4
  print $ recip s4
  print $ recip (recip s4)
  print $ s1 / s4
  print $ s4 / s4
  print $ Series [1, 2, 3] + 1
  print $ compare s1 s2
  print $ compare s2 s1
  print $ compare s1 s5
  print $ compare s3 s4
  print $ s1 < s2
  print $ s1 > s2
  print $ s1 <= s5
  print $ s5 >= s1
  print $ max s1 s5
  print $ min s1 s5
  print $ n1
  print $ n1 + n2
  print $ n1 * n2
  print $ negate n1
  print $ abs n1
  print $ n1 == n2
  print $ n1 == n1
  print $ compare n1 n2
  print $ n1 + 1
  print $ 2 * n1
  print $ n1 * 0.5
  print $ n1 / 2
  print $ recip (Series [Series [2.0], Series [1.0]])
  print $ Series [n1, n2]