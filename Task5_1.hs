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
insertAt dlst i x = list2dlist (insertAt' dlst i x)
insertAt' dlst i x = let (ys,zs) = splitAt i (dlist2list dlst)  
                     in   ys ++ [x] ++ zs  

removeAt :: DList a -> Int -> DList a
removeAt dlst i = list2dlist (removeAt' dlst i)
removeAt' dlst i = let (ys,zs) = splitAt i (dlist2list dlst)    
                   in   ys ++ (tail zs)