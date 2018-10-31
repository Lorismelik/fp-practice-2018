module Task3_1 where

import Prelude hiding (toInteger)

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

instance Show WeirdPeanoNumber where
    show peanoNumber = case normalize peanoNumber of
        Zero -> "Zero"
        Succ child -> concat ["Succ -> ", show child]
        Pred child -> concat ["Pred -> ", show child]


toInteger :: WeirdPeanoNumber -> Integer
toInteger peanoNumber = toInteger' peanoNumber 0
toInteger' peanoNumber num = case (peanoNumber) of
    Zero -> num
    Succ child -> toInteger' child (num + 1)
    Pred child -> if (num - 1 < 0) then error ":(" else toInteger' child (num - 1)


normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize number = let intNumber = toInteger number in
    normalize' Zero intNumber

normalize' :: WeirdPeanoNumber -> Integer -> WeirdPeanoNumber
normalize' number counter = case (counter) of
    0 -> number 
    otherwise -> normalize' (Succ number) (counter - 1)


instance Eq WeirdPeanoNumber where
    (==) p1 p2 = toInteger p1 == toInteger p2

instance Ord WeirdPeanoNumber where
    compare p1 p2
        | toInteger p1 > toInteger p2 = GT
        | toInteger p1 < toInteger p2 = LT
        | toInteger p1 == toInteger p2 = EQ

