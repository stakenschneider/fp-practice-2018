module Task5_2 where
import Todo(todo)
import Data.Ratio
import Data.Fixed

data Stream a = Cons {
                    shead :: a,
                    stail :: Stream a
                }

srepeat :: a -> Stream a
srepeat x =
    let rec = Cons x rec in
    rec

generate :: a -> (a -> a) -> Stream a
generate x f =
    Cons x $ generate (f x) f

instance Functor Stream where
    fmap f (Cons h t) = Cons (f h) (fmap f t)

diag (Cons h t) = Cons (shead h) $ diag (stail <$> t)
sflatten = diag

instance Applicative Stream where
    pure x = srepeat x
    f <*> x = do { f' <- f ; x' <- x ; return $ f' x' }

instance Monad Stream where
    return x = srepeat x
    ss >>= f = sflatten (f <$> ss)


sinPrecisions :: Double -> Stream Double
sinPrecisions x = sinPrecisions' 0 0 (x `sinFun`)  where 
    x = (mod' x (2.0 * pi))

sinFun :: Double -> Double -> Double -> Double
sinFun x i sum = sum + (-1) ** i * x ** p / product[1..p]  where 
    p = (2 * i + 1)

sinPrecisions' :: Double -> Double -> (Double -> Double -> Double) -> Stream Double
sinPrecisions' i sum f  = Cons val $ sinPrecisions' (i + 1) val f where 
    val = i `f` sum

eFun :: Rational -> Rational -> Rational
eFun n sum = sum + 1 / product[1..n]

ePrecisions' :: Rational -> Rational -> Stream Rational
ePrecisions' n sum = Cons val  $ ePrecisions' (n + 1) val
    where val = eFun n sum

ePrecisions :: Stream Rational
ePrecisions = ePrecisions' 0 0