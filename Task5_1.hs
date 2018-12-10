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
    

insertAt :: DList a -> Int -> a -> DList a
insertAt DNil index value = case (index) of 
    0 -> DCons DNil value DNil
    _ -> error(":(")
insertAt (DCons l v r) index value = case (r , index) of
    (_, 1) -> let rec = DCons l v (insertAt' rec r value)
              in rec
    (_, 0) -> let rec = DCons DNil value (insertAt' rec r v)
              in rec
    (DNil, i) | i /= 1 -> error(":(")
    _ -> DCons l v (insertAt r (index - 1) value)

insertAt' :: DList a -> DList a -> a -> DList a
insertAt' left right value = case (right) of
    (DCons l' v' r') -> let rec = DCons left value (insertAt' rec r' v') in rec
    DNil -> DCons left value DNil

removeAt :: DList a -> Int -> DList a
removeAt DNil index = error(":(")
removeAt (DCons l v r)  index = case (r , index) of
    ((DCons l' v' r'), 0) -> case (r') of 
                                (DCons _ v'' r'') -> let rec = DCons l v' (insertAt' rec r'' v'')
                                                     in rec
                                (DNil)            -> DCons l v' DNil               
    (DNil,             0) -> let rec = DNil
                             in rec
    (DNil,             i) | i /= 0 -> error(":(")
    (_,                i) -> DCons l v (removeAt r (index - 1))                      

lst = [4,8,6,7,9]

dlst = list2dlist lst


