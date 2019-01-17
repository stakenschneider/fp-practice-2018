module Task1_2 where
    import Todo(todo)
    import Data.Fixed
    import Prelude hiding (sin, cos, gcd, round)
    
    -- синус числа (формула Тейлора)
    sin :: Double -> Double
    sin x = taylorSin roundedX sum i n where
        roundedX = mod' x (2.0 * pi) :: Double
        n = roundedX :: Double
        sum = 0.0 :: Double
        i = 1 :: Int
    
    taylorSin :: Double -> Double -> Int -> Double -> Double
    taylorSin x sum i n = if (abs n < 1e-8) then sum
        else taylorSin x (sum + n) (i + 1) (sinStep n i x)
            
    sinStep :: Double -> Int -> Double -> Double
    sinStep n i x = n * (-1.0) * x * x / ((2 * (fromIntegral i)) * (2 * (fromIntegral i) + 1))
    
    -- косинус числа (формула Тейлора)
    cos :: Double -> Double
    cos x = taylorCos roundedX sum i n where
        roundedX = mod' x (2.0 * pi) :: Double
        n = 1.0 :: Double
        sum = 0.0 :: Double
        i = 1 :: Int
    
    taylorCos :: Double -> Double -> Int -> Double -> Double
    taylorCos x sum i n = if (abs n < 1e-8) then sum
        else taylorCos x (sum + n) (i + 1) (cosStep n i x)
    
    cosStep :: Double -> Int -> Double -> Double
    cosStep n i x = n * (-1.0) * x * x / ((2 * (fromIntegral i)) * (2 * (fromIntegral i) - 1))

    
    -- наибольший общий делитель двух чисел
    gcd :: Integer -> Integer -> Integer
    gcd x y | y == 0 = abs x 
            | y /= 0 = gcd y (rem x y)
    
    -- существует ли полный целочисленный квадрат в диапазоне [from, to)?
    doesSquareBetweenExist :: Integer -> Integer -> Bool
    doesSquareBetweenExist from to 
        | from == to = False
        | mod' (sqrt (fromIntegral from)) 1 == 0 = True
        | otherwise =  doesSquareBetweenExist (from +  1) to
    
    
    -- является ли дата корректной с учётом количества дней в месяце и
    -- вискокосных годов?
    isDateCorrect :: Integer -> Integer -> Integer -> Bool
    isDateCorrect day month year = todo
    
    -- возведение числа в степень, duh
    -- готовые функции и плавающую арифметику использовать нельзя
    pow :: Integer -> Integer -> Integer
    pow x y
        | y < 0  = error "less than zero"
        | y == 0 = 1
        | y == 1 = x
        | mod' (fromIntegral y) 2 == 0 = pow (x * x) (div y 2)
        | mod' (fromIntegral y) 2 == 1 = x * pow (x * x) (div (y - 1) 2)
    
    -- является ли данное число простым?
    isPrime :: Integer -> Bool
    isPrime x = isPrime' x 2

    isPrime' x i | x == i = True
                 | mod x i == 0 = False
                 | otherwise = isPrime' x (i + 1)
    
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
    