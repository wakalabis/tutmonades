data Termo = Const Int | Div Termo Termo deriving (Show)
data Exception a = Raise String | Return a deriving (Show)

instance Monad Exception where
    return x = Return x
    (Raise e)  >>= f = Raise e
    (Return a) >>= f = f a

raise :: String -> Exception a
raise e = Raise e

--avaliar :: Termo -> Exception Int
--avaliar (Const a) = return a
--avaliar (Div t u) = avaliar t >>= \a ->
--                    avaliar u >>= \b ->
--                    if b == 0
--                      then raise "divisao por zero"
--                      else return (a `div` b)

avaliar :: Termo -> Exception Int
avaliar (Const a) = return a
avaliar (Div t u) = do a <- avaliar t
                       b <- avaliar u
                       if b == 0
                         then raise "divisao por zero"
                         else return (a `div` b)

expr = Div (Div (Const 1932) (Const 2)) (Const 23)
divisaoPorZero = Div (Const 1) (Const 0)

resposta = avaliar expr
erro = avaliar divisaoPorZero
