module Task1_2 where

import Todo(todo)

import Prelude hiding (gcd)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = todo

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y=  
    if x == 0 then y else
        if y == 0 then x else
            if (x < 0) || (y < 0) then gcd (abs x) (abs y) else
                if x > y then gcd (x - y) y
                            else gcd (y - x) x

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = ceiling (sqrt (fromIntegral from)) ^ 2 < to
        
-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = 
    if day <= 0 || month <= 0 || month >= 13 || year <= 0 then False else
        if month == 2 then day < 29 || (((year `rem` 4 == 0 && year `rem` 100 /= 0) || (year `rem` 400 == 0)) && day == 29) else 
            if month `elem` [4, 6, 9, 11] then day < 31 
                else day < 32


-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y =
    if y < 0 then error ("Denominator less than zero") else
        if y == 0 then 1 else 
            if y == 1 then x else
                if y `mod` 2 == 1 then x * (pow x (y - 1))
                    else ((pow x (y `div` 2)) * (pow x (y `div` 2)))

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = foldr (\p r-> p*p>x || (rem x p /= 0 && r)) True primeNums
primeNums = 2 : [x | x <- [3..], isPrime x]


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
