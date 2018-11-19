module Task1_2 where

import Todo(todo)
import Data.Time.Calendar

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd' :: Integer -> Integer -> Integer
gcd' x y | x == y = x
         | x == 0 || y == 0 = 0
         | x > y = gcd' b (a - b)
         | otherwise = gcd' a (b - a)
         where a = abs x
               b = abs y

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to | from > to = False
                               | (isInt $ sqrt $ fromIntegral from) == True = True
                               | otherwise = doesSquareBetweenExist (from + 1)  to

isInt ::  Float -> Bool
isInt x = x == fromInteger (round x) 

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year | fromGregorianValid year (fromIntegral month) (fromIntegral day) == Nothing  = False
                             | otherwise = True

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y | y == 0 = 1
        | y == 1 = x
        | x == 0 = 0
        | otherwise = pow' x x $ abs y

pow' :: Integer -> Integer -> Integer -> Integer
pow' x t 1 = x 
pow' x t y = pow' (t*x) t (y-1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x | (length [n | n <- [2 .. x-1], mod x n == 0]) > 0 = False
          | otherwise = True

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
