data Termo = Const Int | Div Termo Termo deriving (Show)
data Identity a = Identity { runIdentity :: a } deriving (Show)

instance Monad Identity where
    return x = Identity x
    (Identity a) >>= f = f a

--avaliar :: Termo -> Identity Int
--avaliar (Const a) = return a
--avaliar (Div t u) = avaliar t >>= \a ->
--                    avaliar u >>= \b ->
--                    return (a `div` b)

avaliar :: Termo -> Identity Int
avaliar (Const a) = return a
avaliar (Div t u) = do a <- avaliar t
                       b <- avaliar u
                       return (a `div` b)

expr = Div (Div (Const 1932) (Const 2))
           (Const 23)

resposta = avaliar expr
