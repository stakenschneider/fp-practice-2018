module Task3_3 where

import Data.Bits

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

-- Сложение множеств
-- Элемент находится в одном из множеств

newtype PSetOr a = PSetOr{ containsOr :: (a -> Bool) }

instance Semigroup (PSetOr a) where
    (<>) (PSetOr s1) (PSetOr s2) = PSetOr (\x -> s1 x || s2 x)

instance Monoid (PSetOr a) where
    mempty = PSetOr (\x -> False)

-- Пересечение множеств
-- Элемент находится в обоих множествах

newtype PSetAnd a = PSetAnd{ containsAnd :: (a -> Bool) }

instance Semigroup (PSetAnd a) where
    (<>) (PSetAnd s1) (PSetAnd s2) = PSetAnd (\x -> s1 x && s2 x)

instance Monoid (PSetAnd a) where
    mempty = PSetAnd (\x -> False)

-- Разность множеств
-- Элемент находится в одном множестве, но не находится в другом

newtype PSetXor a = PSetXor{ containsXor :: (a -> Bool) }

instance Semigroup (PSetXor a) where
    (<>) (PSetXor s1) (PSetXor s2) = PSetXor (\x -> (/=) (s1 x) (s2 x))

instance Monoid (PSetXor a) where
    mempty = PSetXor (\x -> False)

-- Возможно преобразование A -> B, о В - ничего не знает, поэтому всегда False
instance Functor PSetXor where
    fmap _ _ = PSetXor (\x -> False)