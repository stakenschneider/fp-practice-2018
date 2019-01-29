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
    gcd :: Integral a => a -> a -> a
    gcd 0 0 = error "err"
    gcd m n = gcd' (abs m) (abs n) where
        gcd' m 0 = m
        gcd' m n = gcd' n (rem m n)
    
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

    getX :: Point2D -> Double
    getX  (x, _) = x

    getY :: Point2D -> Double
    getY (_, y) = y

    -- рассчитайте площадь многоугольника по формуле Гаусса
    -- многоугольник задан списком координат
    shapeArea :: [Point2D] -> Double
    shapeArea points = shapeArea' (map getX points) (map getY points)

    shapeArea' :: [Double] -> [Double] -> Double
    shapeArea' x y =  ((0.5*) . abs . sum) $ map det $ zip pxs pys
         where xs = x  ++ [head x]
               ys = y  ++ [head y]
               pxs = zip xs (drop 1 xs)
               pys = zip ys (drop 1 ys)
               det ((x1,x2),(y1,y2)) = x1*y2 - y1*x2

    -- треугольник задан своими координатами.
    -- функция должна вернуть
    --  0, если он тупоугольный
    --  1, если он остроугольный
    --  2, если он прямоугольный
    --  -1, если это не треугольник

    triangleKind :: Point2D -> Point2D -> Point2D -> Integer
    edge ax bx ay by = sqrt((bx - ax)**2 + (by - ay)**2)
    triangleKind a b c = triangleKind' (edge (getX a) (getX b) (getY a) (getY b)) (edge (getX a) (getX c) (getY a) (getY c)) (edge (getX c) (getX b) (getY c) (getY b))
    triangleKind' a b c | (( a + b <= c) || ( a + c <= b) || ( b + c <= a )) = (-1)
                       | ((a * a == b * b + c * c) || (b * b == a * a + c * c) || (c * c == b * b + a * a)) = 2
                       | ((a * a > b * b + c * c) || (b * b > a * a + c * c) || (c * c > b * b + a * a)) = 0
                       | otherwise = 1

