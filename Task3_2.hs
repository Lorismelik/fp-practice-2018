module Task3_2 where

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList rlst = unfoldr rlistToList'  rlst

rlistToList' RNil = Nothing
rlistToList' (RCons prev v) = Just (v, prev)

listToRList :: [a] -> ReverseList a
listToRList lst = foldl RCons RNil lst

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Show a) => Show (ReverseList a) where
    show RNil = "[]"
    show (RCons RNil v) = show v
    show (RCons prev v) = show prev ++ "," ++ show v

instance (Eq a) => Eq (ReverseList a) where
    (==) l r = case (l , r) of
        (RNil, RNil) -> True
        ((RCons lprev lv), (RCons rprev rv)) | lv == rv -> lprev == rprev
        otherwise -> False

instance (Ord a) => Ord (ReverseList a) where
    (<=) l r = case (l , r) of
        (RNil, _) -> True
        (_, RNil) -> False
        ((RCons lprev lv), (RCons rprev rv)) | lv < rv -> True
                                             | lv > rv -> False
                                             | lv == rv -> lprev <= rprev

instance Semigroup (ReverseList a) where
    (<>) rlst RNil = rlst
    (<>) rlst (RCons prev v) = RCons (rlst <> prev) v

instance Monoid (ReverseList a) where
    mempty = RNil

instance Functor ReverseList where
    fmap _ RNil = RNil
    fmap f (RCons prev v) = RCons (fmap f prev) (f v)


b = RCons( RCons( RCons ( RCons (RNil) 5 ) 0) 6) 7
a = [6, 1, 5 ,4]

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
    Just (a, b') -> a : unfoldr f b'
    Nothing -> []