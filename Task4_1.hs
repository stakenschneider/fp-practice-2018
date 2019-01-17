module Task4_1 where
-- Монада над функцией. В качестве входного значения `fun` может быть что угодно
-- Собственно, почему бы не `String`?
data FunMonad a = FunMonad { fun :: String -> a }

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FunMonad`

instance Functor FunMonad where
    fmap f (FunMonad a) = FunMonad(\x -> f (a x))

instance Applicative FunMonad where
    pure a = FunMonad (\x -> a)
    (<*>) (FunMonad f) (FunMonad a) = FunMonad (\x -> (f x) (a x))

instance Monad FunMonad where
    return a = FunMonad(\s -> a)
    (>>=) (FunMonad f) func = FunMonad (\s -> fun (func (f s)) s)