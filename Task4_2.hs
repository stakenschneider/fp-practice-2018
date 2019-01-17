module Task4_2 where
    
data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что 
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

instance Applicative FourOf where
    pure a = FourOf a a a a
    (<*>) (FourOf fa fb fc fd) (FourOf a b c d) = FourOf (fa a) (fb b) (fc c) (fd d)

instance Monad FourOf where
    return a = FourOf a a a a
    (>>=) (FourOf a b c d) f = FourOf (one (f a)) (two (f b)) (three (f c)) (four (f d)) where
        one   (FourOf a _ _ _) = a
        two   (FourOf _ b _ _) = b
        three (FourOf _ _ c _) = c
        four  (FourOf _ _ _ d) = d