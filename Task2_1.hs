module Task2_1 where

    import Todo(todo)
    import Prelude hiding (lookup)
    
    -- Ассоциативный массив на основе бинарного дерева поиска
    -- Ключи - Integer, значения - произвольного типа
    data TreeMap v = Nil 
                    | Node Integer v (TreeMap v) (TreeMap v) Integer
                    deriving(Show, Eq)
    
    
    
    -- Пустое дерево
    emptyTree :: TreeMap v
    emptyTree = Nil
    
    -- Содержится ли заданный ключ в дереве?
    contains :: TreeMap v -> Integer -> Bool
    contains (Node key val l r s) k   | key == k = True
                                      | key > k = contains l k
                                      | key < k = contains r k
    contains Nil _ = False

    
    -- Значение для заданного ключа
    lookup :: Integer -> TreeMap v -> v
    lookup k (Node key val l r s)  | key == k = val
                                   | key > k = lookup k l 
                                   | key < k = lookup k r 
    lookup k Nil = error "Key not found"

    
    -- Вставка пары (ключ, значение) в дерево
    insert :: (Integer, v) -> TreeMap v -> TreeMap v
    insert (k, v) Nil = Node k v Nil Nil 1
    insert (k, v) (Node key val l r s) | key > k = Node key val (insert (k, v) l) r (s + 1)  
                                       | key < k = Node key val l (insert (k, v) r) (s + 1) 
                                       | key == k = error "Duplicate key"
    
    -- Удаление элемента по ключу
    remove :: Integer -> TreeMap v -> TreeMap v
    remove i Nil = error "Key not found"
    remove i (Node key val Nil Nil s) | key == i = Nil
    remove i (Node key val l Nil s) | key == i = l
    remove i (Node key val l r s)       | i < key = Node key val (remove i l) r (s - 1)
                                        | i > key = Node key val l (remove i r) (s - 1)
                                        | otherwise = Node key' (lookup key' r) l (remove key' r) (s - 1) where
                                            key' = takeRight r
    takeRight (Node key _ Nil _ _) = key
    takeRight (Node _ _ l _ _) = takeRight l

    -- Поиск ближайшего снизу ключа относительно заданного
    nearestLE :: Integer -> TreeMap v -> (Integer, v)
    nearestLE i t = nearestLE' i t Nil

    nearestLE' i Nil e = case (e) of
        Nil -> error "Could not find"
        (Node key val l r _ ) -> (key,val)

    nearestLE' i elem@(Node key val l r _ ) e | key == i = (key, val)
                                              | key > i = nearestLE' i l e
                                              | key < i = checkRight r  where
                                                checkRight r' = case (r') of
                                                    Nil -> (key, val)
                                                    otherwise -> nearestLE' i r elem


    -- Построение дерева из списка пар
    treeFromList :: [(Integer, v)] -> TreeMap v
    treeFromList lst = foldr insert Nil lst

    -- Построение списка пар из дерева
    listFromTree :: TreeMap v -> [(Integer, v)]
    listFromTree (Node k v l r _) = listFromTree l ++ [(k, v)] ++ listFromTree r
    listFromTree Nil = []


    -- Поиск k-той порядковой статистики дерева 
    kMean :: Integer -> TreeMap v -> (Integer, v)
    kMean i Nil = error "End of search"
    kMean i (Node k v l r s)   | (sizeOf l) == i = (k, v)
                               | (sizeOf l) > i =  kMean i l
                               | (sizeOf l) < i =  kMean (i - (sizeOf l) - 1) r
        
    sizeOf :: TreeMap v -> Integer
    sizeOf Nil = 0
    sizeOf (Node _ _ _ _ s) = s
    
    nums = [(8, 0),(6, 0),(4, 0)]
    
    tree = treeFromList nums