-- Шаблон для выполнения заданий Лабораторной работы №4

-- ниже перечисляются имена, доступные после загрузки данного модуля
-- (например в файле с тестами)
-- по мере реализации решений заданий снимайте комментарий
-- с соответствующей функции
module Lab4
  ( epsilon
  , epsilon'
  , epsilon''
  , nat
  , fibonacci
  , factorial
  , hailstone
  , hailstoneStepNum
  , hailstonePeak
  , powers
  , findCloseEnough
  , streamSum
  , expSummands
  , expStream
  , expAppr
  , derivativeAppr
  , derivativeStream
  , derivative
  , funAkStream
  , invF
  , average
  , averageDump
  , newtonTransform
  , eitken
  , fixedPoint
  , fixedPointOfTransform
  , sqrt1
  , cubert1
  , sqrt2
  , cubert2
  , extremum
  , myPi
  ) where
--------------------------------------------------------------------------------
-- Вспомогательные определения
--------------------------------------------------------------------------------
import Data.List
epsilon  = 0.001
epsilon' = 0.00001
epsilon'' = 0.00000001
--------------------------------------------------------------------------------

-- Задание 1 nat
-- Все натуральные числа: 1, 2, 3, ...
nat = [1 ..]

-- Задание 2 fibonacci
-- Числа Фибоначчи: после 0 и 1 каждый элемент равен сумме двух предыдущих
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

-- Задание 3 factorial
-- Факториалы 0!, 1!, 2!, ...: n-й элемент получается умножением числа n
-- на предыдущий элемент этой же последовательности
factorial = 1 : zipWith (*) nat factorial

-- Задание 4 hailstone
-- Бесконечная последовательность чисел-градин, начинающаяся с x
hailstone x = iterate next x
  where next n = if even n then n `div` 2 else 3 * n + 1

-- Задание 5 hailstoneStepNum
-- Количество шагов, за которое последовательность градин от x доходит
-- до первой единицы (индекс первой единицы в последовательности)
hailstoneStepNum x = n
  where Just n = elemIndex 1 (hailstone x)

-- Задание 6 hailstonePeak
-- Максимальное значение последовательности градин от x (x > 1),
-- рассматриваемой до первого появления единицы
hailstonePeak x = maximum (takeWhile (/= 1) (hailstone x))

-- Задание 7 powers
-- Степени x: на i-й позиции стоит x в степени i
powers x = iterate (* x) 1

-- Задание 8 streamSum
-- Частичные суммы: на i-й позиции стоит сумма первых i элементов stream
streamSum stream = sums
  where sums = 0 : zipWith (+) stream sums

-- Задание 9 findCloseEnough
-- Первый элемент последовательности xs, отличающийся по модулю от
-- предыдущего элемента не более чем на eps
findCloseEnough eps xs = found
  where Just (_, found) = find close (zip xs (tail xs))
        close (prev, next) = abs (next - prev) <= eps

--  Задание 10 expSummands
-- Слагаемые ряда для e в степени x: на i-й позиции стоит x^i / i!
expSummands x = zipWith (/) (powers x) (map fromIntegral factorial)

--  Задание 11 expStream
-- Последовательность приближений к e в степени x
expStream x = streamSum (expSummands x)

--  Задание 12 expAppr
-- Значение e в степени x с точностью eps
expAppr eps x = findCloseEnough eps (expStream x)

--  Задание 13 derivativeAppr
-- Приближённое значение производной функции f в точке x для приращения dx
derivativeAppr f dx x = (f (x + dx) - f x) / dx

--  Задание 14 derivativeStream
-- Приближения производной функции f для приращений 1, 1/2, 1/4, ...
derivativeStream f = map (derivativeAppr f) (iterate (/ 2) 1)

--  Задание 15 derivative
-- Производная функции f: значение в точке x берётся из последовательности
-- приближений с точностью epsilon'
derivative f x = findCloseEnough epsilon' approximations
  where approximations = map (\g -> g x) (derivativeStream f)

--  Задание 16 funAkStream
-- Последовательность функций A0, A1, ..., где A0(x) = x,
-- а A(k)(x) = (A(k-1))'(x) / g(x)
funAkStream g = iterate next (\x -> x)
  where next a x = derivative a x / g x

--  Задание 17 invF
-- Приближение значения обратной функции для f в точке x при начальном
-- приближении y0: сумма степенного ряда с точностью epsilon
invF f y0 x = findCloseEnough epsilon (streamSum products)
  where coefficients = map (\a -> a y0) (funAkStream (derivative f))
        summands = expSummands (x - f y0)
        products = zipWith (*) coefficients summands

--  Задание 18 average
-- Среднее арифметическое двух чисел
average x y = (x + y) / 2

--  Задание 19 averageDump
-- Среднее арифметическое между x и значением f в x
averageDump f x = average x (f x)

--  Задание 20 newtonTransform
-- Преобразование Ньютона: x - g(x) / g'(x)
newtonTransform g x = x - g x / derivative g x

--  Задание 21 eitken
-- Ускорение последовательности по формуле Эйткена: каждая тройка соседних
-- элементов (a, b, c) даёт один элемент (c * a - b^2) / (c - 2 * b + a)
eitken s = map aitken (zip s (zip (tail s) (tail (tail s))))
  where aitken (a, (b, c)) = (c * a - b * b) / (c - 2 * b + a)

--  Задание 22 fixedPoint
-- Последовательность приближений к неподвижной точке f: x0, f x0, f (f x0)
fixedPoint f x0 = iterate f x0

--  Задание 23 fixedPointOfTransform
-- Неподвижная точка преобразования g функции f с точностью epsilon'
-- при начальном приближении x0
fixedPointOfTransform f g x0 = findCloseEnough epsilon' (fixedPoint (g f) x0)

--  Задание 24 sqrt1
-- Квадратный корень как неподвижная точка y -> x / y с усреднением
sqrt1 x = fixedPointOfTransform (x /) averageDump 1.0

--  Задание 25 cubert1
-- Кубический корень как неподвижная точка y -> x / y^2 с усреднением
cubert1 x = fixedPointOfTransform (\y -> x / (y * y)) averageDump 1.0

--  Задание 26 sqrt2
-- Квадратный корень как нуль функции y^2 - x (метод Ньютона)
sqrt2 x = fixedPointOfTransform (\y -> y * y - x) newtonTransform 1.0

--  Задание 27 cubert2
-- Кубический корень как нуль функции y^3 - x (метод Ньютона)
cubert2 x = fixedPointOfTransform (\y -> y * y * y - x) newtonTransform 1.0

--  Задание 28 extremum
-- Точка, подозрительная на экстремум (нуль первой производной, найденный
-- методом Ньютона из точки 1.0), и её тип по знаку второй производной
extremum f = (x, kind)
  where x = fixedPointOfTransform (derivative f) newtonTransform 1.0
        d2 = derivative (derivative f) x
        kind = if abs d2 <= epsilon then "inflection"
               else if d2 > 0 then "minimum"
               else "maximum"

--  Задание 29 myPi
-- Число пи: сумма ряда Лейбница, ускоренная формулой Эйткена,
-- с точностью epsilon''
myPi = 4 * findCloseEnough epsilon'' (eitken (streamSum terms))
  where terms = zipWith (/) (powers (-1)) (iterate (+ 2) 1)