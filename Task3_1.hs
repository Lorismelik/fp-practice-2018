module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

instance Show WeirdPeanoNumber where
    show peanoNumber = case normalize peanoNumber of
        Zero -> "Zero"
        Succ child -> concat ["Succ -> ", show child]
        Pred child -> concat ["Pred -> ", show child]


wpnToInteger :: WeirdPeanoNumber -> Integer
wpnToInteger peanoNumber = case (peanoNumber) of
    Zero -> 0
    Succ prev -> wpnToInteger prev + 1
    Pred prev -> wpnToInteger prev - 1


normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize number = let intNumber = wpnToInteger number in
    normalize' Zero intNumber

normalize' :: WeirdPeanoNumber -> Integer -> WeirdPeanoNumber
normalize' number counter = case (counter) of
    0 -> number 
    otherwise | counter > 0 -> normalize' (Succ number) (counter - 1)
              | counter < 0 -> normalize' (Pred number) (counter + 1)


instance Eq WeirdPeanoNumber where
    (==) p1 p2 = wpnToInteger p1 == wpnToInteger p2

instance Ord WeirdPeanoNumber where
    compare p1 p2
        | wpnToInteger p1 > wpnToInteger p2 = GT
        | wpnToInteger p1 < wpnToInteger p2 = LT
        | wpnToInteger p1 == wpnToInteger p2 = EQ

add :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
add p1 p2  = case p1 of
    Zero -> p2
    Succ prev -> Succ $ add prev p2
    Pred prev -> Pred $ add prev p2

mult :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
mult p1 p2 = case (p1, p2) of
    (Zero, _) -> Zero
    (_, Zero) -> Zero
    otherwise -> mult' p (abs coef)
        where coef = wpnToInteger p2
              p | coef > 0 = p1
                | coef < 0 = negate p1

mult' :: WeirdPeanoNumber -> Integer -> WeirdPeanoNumber
mult' p coef  = case coef of
    0 -> Zero
    _ -> add p $ (mult' p (coef - 1)) 


negation :: WeirdPeanoNumber -> WeirdPeanoNumber
negation p = case p of 
    Zero -> Zero
    Succ prev -> Pred $ negation prev
    Pred prev -> Succ $ negation prev 

absolute :: WeirdPeanoNumber -> WeirdPeanoNumber
absolute p | p < Zero = negate p
           | otherwise = p

sign :: WeirdPeanoNumber -> WeirdPeanoNumber
sign p = case p of 
    Zero -> Zero
    Succ _ -> Succ Zero
    Pred _ -> Pred Zero

wpnFromInteger :: Integer -> WeirdPeanoNumber
wpnFromInteger p | p == 0 = Zero
                 | p > 0 = Succ $ wpnFromInteger (p - 1)
                 | p < 0 = Pred $ wpnFromInteger (p + 1)
    
instance Num WeirdPeanoNumber where
    (+) p1 p2 = add p1 p2
    (*) p1 p2 = mult (normalize p1) (normalize p2)
    negate p =  negation p
    abs p = absolute p
    signum p = sign (normalize p)
    fromInteger p  = wpnFromInteger p 

wpnToRational :: WeirdPeanoNumber -> Rational
wpnToRational peanoNumber = case peanoNumber of 
    Zero -> 0
    Succ prev -> toRational prev + 1
    Pred prev -> toRational prev - 1

instance Real WeirdPeanoNumber where
    toRational p = wpnToRational p 

wpnToEnum :: Int -> WeirdPeanoNumber
wpnToEnum p | p == 0 = Zero
            | p > 0 = Succ $ wpnToEnum (p - 1)
            | p < 0 = Pred $ wpnToEnum (p + 1)

wpnFromEnum :: WeirdPeanoNumber -> Int
wpnFromEnum p = case p of
    Zero -> 0
    Succ prev -> wpnFromEnum prev + 1
    Pred prev -> wpnFromEnum prev - 1

instance Enum WeirdPeanoNumber where
    succ p = Succ p
    pred p = Pred p
    toEnum p = wpnToEnum p
    fromEnum p = wpnFromEnum p

wpnQuotRem :: WeirdPeanoNumber -> WeirdPeanoNumber -> (WeirdPeanoNumber, WeirdPeanoNumber)
wpnQuotRem p1 p2 = case (p1, p2) of
    (Zero, _) -> (Zero, Zero)
    (_, Zero) -> error ":("
    otherwise -> (res, p1 - p2 * res)
        where s | p2 > 0 = p1
                | p2 < 0 = negate p1
              res = wpnQuotRem' (abs p1) (abs p2) s

wpnQuotRem' :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
wpnQuotRem' p1 p2 s | p1 >= p2 = if (s < 0 ) then Pred (wpnQuotRem' (p1 - p2) p2 s) else Succ (wpnQuotRem' (p1 - p2) p2 s)
                    | otherwise = Zero
                                                        

instance Integral WeirdPeanoNumber where
    toInteger p = wpnToInteger p
    quotRem p1 p2 = wpnQuotRem (normalize p1) (normalize p2)






f = Zero
f' = Succ f
f'' = Succ f'
f''' = Succ f''
f'''' = Succ f'''

s = Zero
s' = Succ s
s'' = Succ s'