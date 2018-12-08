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

index :: DList a -> Int -> a
index dlst i = case (i, dlst) of
    (0, (DCons _ v _)) -> v
    (_, (DCons _ _ r)) -> index r (i - 1)
    otherwise -> error(":(") 
    
indexElem :: DList a -> Int -> Dlist a
index dlst i = case (i, dlst) of
    (0, (DCons _ v _)) -> dlst
    (_, (DCons _ _ r)) -> index r (i - 1)
    otherwise -> error(":(") 

insertAt :: DList a -> Int -> a -> DList a
insertAt DNil index value = DCons DNil value DNil
insertAt (DCons _ _ _) index value = insertAt' (indexElem list i) value

insertAt' (DCons l v r) value = 

removeAt :: DList a -> Int -> DList a
removeAt list index = todo

lst = [4,8,6,7,9]

dlst = list2dlist lst


