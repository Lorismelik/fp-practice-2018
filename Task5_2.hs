module Task5_2 where

import Todo(todo)
import Data.Ratio
import Prelude hiding (round)

data Stream a = Cons {
                    shead :: a,
                    stail :: Stream a
                }

srepeat :: a -> Stream a
srepeat x =
    let rec = Cons x rec in
    rec

generate :: a -> (a -> a) -> Stream a
generate x f =
    Cons x $ generate (f x) f

instance Functor Stream where
    fmap f (Cons h t) = Cons (f h) (fmap f t)

diag (Cons h t) = Cons (shead h) $ diag (stail <$> t)
sflatten = diag

instance Applicative Stream where
    pure x = srepeat x
    f <*> x = do { f' <- f ; x' <- x ; return $ f' x' }

instance Monad Stream where
    return x = srepeat x
    ss >>= f = sflatten (f <$> ss)


sinPrecisions :: Double -> Stream Double
sinPrecisions x = sinPrecisions' (round x (2.0 * pi)) 0 0

sinFun :: Double -> Double -> Double -> Double
sinFun x n s = s + (-1) ** n * x ** p / product[1..p]  where 
    p = (2 * n + 1)

sinPrecisions' :: Double -> Double -> Double -> Stream Double
sinPrecisions' x n s = Cons (sinFun x n s) $ sinPrecisions' x (n + 1) (sinFun x n s)

eFun :: Rational -> Rational -> Rational
eFun n s = s + 1 / product[1..n]

ePrecisions' :: Rational -> Rational -> Stream Rational
ePrecisions' n s = Cons (eFun n s)  $ ePrecisions' (n + 1) (eFun n s)

ePrecisions :: Stream Rational
ePrecisions = ePrecisions' 0 0

round :: Double -> Double -> Double
round x y = x - (fromIntegral f) * y where 
    f = floor (x / y)


