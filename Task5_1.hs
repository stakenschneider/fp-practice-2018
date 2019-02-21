module Task5_1 where

    import Todo(todo)

    data DList a = DNil
                 | DCons {
                    left :: (DList a),
                    current :: a,
                    right :: (DList a)
                 }

    instance (Show a) => Show (DList a) where
        show it = "[" ++ showBody it ++ "]"
                  where showBody DNil = ""
                        showBody (DCons _ h DNil) = show h
                        showBody (DCons _ h t) = show h ++ ", " ++ showBody t

    instance (Eq a) => Eq (DList a) where
        DNil == DNil = True
        (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
        _ == _ = False

    list2dlist :: [a] -> DList a
    list2dlist lst = list2dlist' DNil lst

    list2dlist' :: DList a -> [a] -> DList a
    list2dlist' _ [] = DNil
    list2dlist' left (h: t) =
        let rec = DCons left h (list2dlist' rec t)
        in rec

    dlist2list :: DList a -> [a]
    dlist2list dlst = unfoldr dlist2list' dlst

    dlist2list' DNil = Nothing
    dlist2list' (DCons _ v r) = Just(v, r)

    unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
    unfoldr f b = case f b of
        Just (a, b') -> a : unfoldr f b'
        Nothing -> []


    index :: DList a -> Int -> a
    index DNil ind = error "error: list is empty"
    index (DCons left current right) 0 = current
    index (DCons left current right) ind = index right (ind - 1)

    insertAt :: DList a -> Int -> a -> DList a
    insertAt dlst i x | i > 0 = insertAt' DNil dlst (i-1) x
                      | i == 0 = insertAt'' DNil x dlst

    insertAt' :: DList a -> DList a -> Int -> a -> DList a
    insertAt' left (DCons l c r) i x | i == 0   = let elem = DCons left c (insertAt'' elem x r)
                                                            in elem
                                     | otherwise = let elem = DCons left c (insertAt' elem r (i-1) x)
                                                            in elem
    insertAt'' :: DList a -> a -> DList a -> DList a
    insertAt'' left x (DCons l c r) = let elem = DCons left x (insertAt'' elem c r) in elem
    insertAt'' left x DNil = DCons left x DNil

    removeAt :: DList a -> Int -> DList a
    removeAt dlst@(DCons l c r) i   | i > 0 = removeAt' DNil dlst i
                                    | i == 0 = case (r) of
                                                DNil             -> DNil
                                                (DCons l' c' r') -> removeAt'' DNil c' r'

    removeAt' left (DCons l c r) i | i == 0  =  case (r) of
                                                DNil             -> DNil
                                                (DCons l' c' r') -> removeAt'' left c' r'

                                   | otherwise = let elem = DCons left c (removeAt' elem r (i-1)) in elem

    removeAt'' left x (DCons l c r)  = let elem = DCons left x (removeAt'' elem c r) in elem
    removeAt'' left x DNil = DCons left x DNil

    dlst = list2dlist [1, 2, 3, 4, 5 ,6]