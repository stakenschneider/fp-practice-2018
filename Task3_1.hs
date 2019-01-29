module Task3_1 where

    data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber
    
    -- Реализуйте все классы типов, которым должны отвечать целые числа
    
    instance Show WeirdPeanoNumber where
        show Zero = "Zero"
        show (Succ x)  = concat ["Succ => ", show x]
        show (Pred x) = concat ["Pred => ", show x]
    
    
    wpnToInteger :: WeirdPeanoNumber -> Integer
    wpnToInteger  Zero = 0
    wpnToInteger  (Succ x) = wpnToInteger x + 1
    wpnToInteger  (Pred x) = wpnToInteger x - 1
    
    
    normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
    normalize wpn = wpnFromInteger (wpnToInteger wpn)
    
    wpnFromInteger :: Integer -> WeirdPeanoNumber
    wpnFromInteger p | p == 0 = Zero
                     | p > 0 = Succ $ wpnFromInteger (p - 1)
                     | p < 0 = Pred $ wpnFromInteger (p + 1)                  
    
    
    instance Eq WeirdPeanoNumber where
        (==) wpn1 wpn2 = wpnToInteger wpn1 == wpnToInteger wpn2
    
    instance Ord WeirdPeanoNumber where
        (<=) wpn1 wpn2 = (wpnToInteger wpn1) <= (wpnToInteger wpn2)
    
    add :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
    add  Zero wpn2 = wpn2
    add  (Succ x) wpn2 = add x (Succ wpn2)
    add  (Pred x) wpn2 = add x (Pred wpn2)

    mult :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
    mult p1 p2 = case (p1, p2) of
        (Zero, _) -> Zero
        (_, Zero) -> Zero
        otherwise -> mult' p p2 where
            p = case (sign p2) of
                (Succ _) -> p1
                (Pred _) -> negate p1

    mult' :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
    mult' p1 p2  = case p2 of
        Zero -> Zero
        (Succ x) -> add p1 $ (mult' p1 x)
        (Pred x) -> add p1 $ (mult' p1 x)


    negation :: WeirdPeanoNumber -> WeirdPeanoNumber
    negation  Zero = Zero
    negation  (Succ x) = Pred (negation x)
    negation  (Pred x) = Succ (negation x)

    absolute :: WeirdPeanoNumber -> WeirdPeanoNumber
    absolute p | p < Zero = negate p
               | otherwise = p

    sign :: WeirdPeanoNumber -> WeirdPeanoNumber
    sign Zero = Zero
    sign (Succ _) = Succ Zero
    sign (Pred _) = Pred Zero

    instance Num WeirdPeanoNumber where
        (+) wpn1 wpn2 = add wpn1 wpn2
        (*) wpn1 wpn2 = mult (normalize wpn1) (normalize wpn2)
        negate = negation
        abs = absolute
        signum wpn = sign (normalize wpn)
        fromInteger = wpnFromInteger

    wpnToRational :: WeirdPeanoNumber -> Rational
    wpnToRational  Zero = 0
    wpnToRational  (Succ x) = toRational x + 1
    wpnToRational  (Pred x) = toRational x - 1

    instance Real WeirdPeanoNumber where
        toRational wpn = wpnToRational wpn

    wpnToEnum :: Int -> WeirdPeanoNumber
    wpnToEnum wpn | wpn == 0 = Zero
                  | wpn > 0 = Succ (wpnToEnum (wpn - 1))
                  | wpn < 0 = Pred (wpnToEnum (wpn + 1))

    wpnFromEnum :: WeirdPeanoNumber -> Int
    wpnFromEnum  Zero = 0
    wpnFromEnum  (Succ x) = wpnFromEnum x + 1
    wpnFromEnum  (Pred x) = wpnFromEnum x - 1

    instance Enum WeirdPeanoNumber where
        succ p = Succ p
        pred p = Pred p
        toEnum p = wpnToEnum p
        fromEnum p = wpnFromEnum p

    wpnQuotRem :: WeirdPeanoNumber -> WeirdPeanoNumber -> (WeirdPeanoNumber, WeirdPeanoNumber)
    wpnQuotRem wpn1 wpn2 = case (wpn1, wpn2) of
        (Zero, _) -> (Zero, Zero)
        (_, Zero) -> error "Division by zero"
        otherwise -> (res, wpn1 - wpn2 * res) where
                  res = wpnQuotRem' (abs wpn1) (abs wpn2) (sign wpn2)

    wpnQuotRem' :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
    wpnQuotRem' wpn1 wpn2 s | wpn1 >= wpn2 = case (s) of
                                            (Succ _) -> Succ (wpnQuotRem' (wpn1 - wpn2) wpn2 s)
                                            (Pred _) -> Pred (wpnQuotRem' (wpn1 - wpn2) wpn2 s)
                            | otherwise = Zero

    instance Integral WeirdPeanoNumber where
        toInteger wpn = wpnToInteger wpn
        quotRem wpn1 wpn2 = wpnQuotRem (normalize wpn1) (normalize wpn2)
