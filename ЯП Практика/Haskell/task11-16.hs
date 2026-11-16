f11 x eps n = take (n + 1) lst
        where
          lst = 1 : x : elemLst 1 lst
          c = (x + 7) / (x * x)
          elemLst k (y0 : ys@(y1 : _))
            | abs (y0 - y1) < eps = []
            | otherwise = c / k * y1 * y1 * y1 - y0 / (k * k)
                          : elemLst (k + 1) ys

main = do
  print $ f11 (-7) 0.01 0
  print $ f11 (-7) 0.001 1
  print $ f11 (-7) 0.001 3
  print $ f11 1 0.001 500
  print $ f11 (-7) 0.01 50
  print $ f11 (-8) 0.05 50
  print $ f11 (-6) 0.0001 100
  print $ f11 2 0.001 4