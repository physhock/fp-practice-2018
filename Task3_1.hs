module Task3_1 where

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа

instance Show WeirdPeanoNumber where
    show Zero = "Zero"
    show (Succ a) = "Succ " ++ show a
    show (Pred a) = "Pred " ++ show a

instance Eq WeirdPeanoNumber where
    (==) a b = toInteger' a == toInteger' b

instance Ord WeirdPeanoNumber where
    compare a b = compare' (normalize a) (normalize b)

instance Enum WeirdPeanoNumber where
    toEnum a = fromInteger' $ toInteger a
    fromEnum a = fromIntegral $ toInteger' a  

instance Num WeirdPeanoNumber where
    (+) a b = add a b
    (*) a b = mult a b
    abs = abs'
    signum = signum'
    negate = negate'
    fromInteger = fromInteger'

instance Real WeirdPeanoNumber where
    toRational a = toRational $ toInteger' a  

instance Integral WeirdPeanoNumber where
    toInteger = toInteger'
    quotRem a b = quotRem' a b

toInteger' :: WeirdPeanoNumber -> Integer
toInteger' Zero = 0
toInteger' (Succ a) = toInteger' a + 1
toInteger' (Pred a) = toInteger' a - 1

fromInteger' :: Integer -> WeirdPeanoNumber
fromInteger' a | a == 0 = Zero
               | a > 0 = Succ $ fromInteger' $ a - 1
               | a < 0 = Pred $ fromInteger' $ a + 1

normalize :: WeirdPeanoNumber -> WeirdPeanoNumber
normalize a = fromInteger' $ toInteger' a

compare' :: WeirdPeanoNumber -> WeirdPeanoNumber -> Ordering
compare' a b = case (a, b) of
                (Zero, Zero) -> EQ
                (Zero, Succ s) -> LT
                (Zero, Pred s) -> GT
                (Succ f, Zero) -> GT
                (Pred f, Zero) -> LT
                (Succ f, Succ s) -> compare' f s
                (Pred f, Pred s) -> compare' f s
                (Succ f, _) -> GT
                (Pred f, _) -> LT

add :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
add Zero b = b
add (Succ a) b = Succ $ a + b
add (Pred a) b = Pred $ a + b

abs' :: WeirdPeanoNumber -> WeirdPeanoNumber
abs' a | a > 0 = a
       | a < 0 = negate' a
       | otherwise = Zero

signum' :: WeirdPeanoNumber -> WeirdPeanoNumber
signum' Zero = Zero
signum' (Succ _) = Succ Zero
signum' (Pred _) = Pred Zero

negate' :: WeirdPeanoNumber -> WeirdPeanoNumber
negate' Zero = Zero
negate' (Succ a) = Pred $ negate' a
negate' (Pred a) = Succ $ negate' a 

mult :: WeirdPeanoNumber -> WeirdPeanoNumber -> WeirdPeanoNumber
mult Zero _ = Zero
mult _ Zero = Zero
mult a b | signum' a == signum' b = mult' (abs' a) (toInteger' $ abs' b)
         | otherwise = negate' $ mult' (abs' a) (toInteger' $ abs' b)
        where 
            mult' f 0 = Zero
            mult' f s = f + (f `mult'` ( s - 1 ))

quotRem' :: WeirdPeanoNumber -> WeirdPeanoNumber -> (WeirdPeanoNumber, WeirdPeanoNumber)
quotRem' Zero _ = (Zero, Zero)
quotRem' _ Zero = error "division by zero"
quotRem' a b = case (signum a, signum b) of
                (Succ Zero, Succ Zero) -> quotRem'' a b 0
                (Succ Zero, Pred Zero) -> negateQuot $ quotRem'' a (negate' b) 0
                (Pred Zero, Succ Zero) -> negateQuot $ negateRem $ quotRem'' (negate' a) b 0
                (Pred Zero, Pred Zero) -> negateRem $ quotRem'' (negate' a) (negate' b) 0
                where
                    negateQuot (q, r) = (negate' q, r)
                    negateRem (q, r) = (q, negate' r)
                    quotRem'' a b s | a >= b = quotRem'' (a - b) b (s + 1)
                                    | otherwise = (normalize s, normalize a)