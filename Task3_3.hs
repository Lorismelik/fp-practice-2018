module Task3_3 where

newtype PSet a = PSet{ contains :: (a -> Bool) }

-- Реализуйте классы Monoid и Functor
-- Объясните в комментариях, почему они реализованы именно так

-- Объединение множеств
-- Элемент находится в одном из множеств

newtype PSetUnion a = PSetUnion { containsUnion :: (a -> Bool) }

instance Semigroup (PSetUnion  a) where
    (<>) (PSetUnion s1) (PSetUnion  s2) = PSetUnion  (\x -> s1 x || s2 x)

instance Monoid (PSetUnion  a) where
    mempty = PSetUnion (\x -> False)

-- Пересечение множеств
-- Элемент находится в обоих множествах

newtype PSetIntersect a = PSetIntersect{ containsIntersect :: (a -> Bool) }

instance Semigroup (PSetIntersect a) where
    (<>) (PSetIntersect s1) (PSetIntersect s2) = PSetIntersect (\x -> s1 x && s2 x)

instance Monoid (PSetIntersect a) where
    mempty = PSetIntersect (\x -> False)

-- Разность множеств
-- Элемент находится в одном множестве, но не находится в другом

newtype PSetOuter a = PSetOuter{ containsOuter :: (a -> Bool) }

instance Semigroup (PSetOuter a) where
    (<>) (PSetOuter s1) (PSetOuter s2) = PSetOuter (\x -> (/=) (s1 x) (s2 x))
        
instance Monoid (PSetOuter a) where
    mempty = PSetOuter (\x -> False)

instance Functor PSet where
    fmap f (PSet contains) =  PSet(f . contains)