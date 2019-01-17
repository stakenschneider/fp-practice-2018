module Task3_2 where

    import Todo(todo)
    
    data ReverseList a = RNil | RCons (ReverseList a) a
    
    rlistToList :: ReverseList a -> [a]
    rlistToList RNil = []
    rlistToList (RCons prev val) = (rlistToList prev) ++ [val]

    
    listToRList :: [a] -> ReverseList a
    listToRList [] = RNil
    listToRList (h:t) = RCons (listToRList t) h
    
    -- Реализуйте классы Eq, Ord, Show, Monoid, Functor
    
    instance (Show a) => Show (ReverseList a) where
        show RNil = "[]"
        show (RCons RNil v) = show v
        show (RCons prev v) = show prev ++ "->" ++ show v


    equal RNil RNil = True
    equal l r = case (l, r) of 
        ((RCons lprev lv), (RCons rprev rv)) | lv == rv -> lprev == rprev
        otherwise -> False    
    
    lessEqual RNil _ = True
    lessEqual _ RNil = False
    lessEqual (RCons lprev lv) (RCons rprev rv) | lv < rv -> True
                                                | lv > rv -> False
                                                | lv == rv -> lprev <= rprev   
    
    instance (Eq a) => Eq (ReverseList a) where
        (==) l r = equal l r

    instance (Ord a) => Ord (ReverseList a) where
        (<=) l r = lessEqual l r
    
    instance Semigroup (ReverseList a) where
        (<>) rlst RNil = rlst
        (<>) rlst (RCons prev v) = RCons (rlst <> prev) v
    
    instance Monoid (ReverseList a) where
        mempty = RNil
    
    instance Functor ReverseList where
        fmap _ RNil = RNil
        fmap f (RCons prev v) = RCons (fmap f prev) (f v)