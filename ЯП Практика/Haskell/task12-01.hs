f12 x = seqn
  where
    seqn = x : map sumSquareDigits seqn
    sumSquareDigits n = digitSum (n * n)
    digitSum n
      | n < 10 = n
      | otherwise = n `mod` 10 + digitSum (n `div` 10)

main = do
  print $ take 6 $ f12 136
  print $ take 6 $ f12 0
  print $ take 6 $ f12 1
  print $ take 10 $ f12 99
  print $ take 10 $ f12 (-7)
  print $ take 10 $ f12 987654321
  print $ drop 100 $ take 105 $ f12 12345