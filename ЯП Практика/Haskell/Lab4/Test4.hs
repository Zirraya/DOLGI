import Lab4

main = do
  -- задания 1-3: простые бесконечные последовательности
  print $ take 10 nat
  print $ take 12 fibonacci
  print $ take 8 factorial
  -- задания 4-6: числа-градины
  print $ take 10 (hailstone 7)
  print $ hailstoneStepNum 4
  print $ hailstoneStepNum 27
  print $ hailstonePeak 27
  -- задания 7-9: степени, частичные суммы, поиск близких элементов
  print $ take 6 (powers 3)
  print $ take 6 (powers 0.5)
  print $ take 6 (streamSum nat)
  print $ findCloseEnough 0.01 (map (\n -> 1 / fromIntegral n) nat)
  -- задания 10-12: экспонента
  print $ take 5 (expSummands 2.0)
  print $ take 5 (expStream 1.0)
  print $ expAppr epsilon 1.0
  print $ expAppr epsilon' 2.0
  -- задания 13-15: производная
  print $ derivativeAppr (\x -> x * x) 0.001 3.0
  print $ map (\g -> g 1.0) (take 4 (derivativeStream sin))
  print $ derivative sin 1.0
  print $ derivative (\x -> x * x * x) 2.0
  -- задания 16-17: обратная функция
  print $ map (\a -> a 1.0) (take 3 (funAkStream (\x -> 2 * x)))
  print $ invF sin 0.5 0.45
  print $ invF (\x -> x * x) 3.0 10.0
  -- задания 18-20: усреднение и преобразование Ньютона
  print $ average 2.0 5.0
  print $ averageDump (\x -> x * x) 3.0
  print $ newtonTransform (\x -> x * x - 2) 1.0
  -- задания 21-23: ускорение Эйткена и неподвижные точки
  print $ take 4 (eitken (take 6 (streamSum (map (1 /) (powers 2.0)))))
  print $ take 4 (fixedPoint cos 1.0)
  print $ fixedPointOfTransform (\x -> x * x - 4) newtonTransform 1.0
  -- задания 24-27: корни
  print $ sqrt1 2.0
  print $ cubert1 27.0
  print $ sqrt2 2.0
  print $ cubert2 27.0
  -- задание 28: экстремумы
  print $ extremum (\x -> (x - 3) * (x - 3))
  print $ extremum (\x -> 1 - (x - 2) * (x - 2))
  print $ extremum (\x -> x * x * x - 3 * x)
  -- задание 29: число пи
  print myPi