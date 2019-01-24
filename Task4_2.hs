module Task4_2 where

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что 
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

instance Functor FourOf where
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

instance Applicative FourOf where
    pure a = FourOf a a a a
    (<*>) (FourOf a b c d) (FourOf e f g h) = FourOf (a e) (b f) (c g) (d h)

--now `(+) <$> g <*> j` works (g + j)

instance Monad FourOf where
    return a =  FourOf a a a a
    (>>=) (FourOf a b c d) f = FourOf (x1 (f a)) (x2 (f b)) (x3 (f c)) (x4 (f d))
        where
            x1 (FourOf z1 _ _ _) = z1
            x2 (FourOf _ z2 _ _) = z2
            x3 (FourOf _ _ z3 _) = z3
            x4 (FourOf _ _ _ z4) = z4

--do{...} works