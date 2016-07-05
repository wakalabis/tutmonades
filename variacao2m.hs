data Termo = Const Int | Div Termo Termo deriving (Show)
data State a = State { runState :: (Int -> (a, Int)) }

instance Monad State where
    return x = State (\s -> (x, s))
    m >>= k  = State $ \x -> let (a, y) = runState m x
                                 (b, z) = runState (k a) y
                             in (b, z)

tick :: State ()
tick = State (\s -> ((), s + 1))

--avaliar :: Termo -> State Int
--avaliar (Const a) = return a
--avaliar (Div t u) = avaliar t >>= \a ->
--                    avaliar u >>= \b ->
--                    tick      >>= \_ ->
--                    return (a `div` b)

avaliar :: Termo -> State Int
avaliar (Const a) = return a
avaliar (Div t u) = do a <- avaliar t
                       b <- avaliar u
                       tick
                       return (a `div` b)

expr = Div (Div (Const 1932) (Const 2)) (Const 23)
resposta = runState (avaliar expr) 0
