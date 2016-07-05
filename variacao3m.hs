data Termo = Const Int | Div Termo Termo deriving (Show)
data Writer a = Writer { runWriter :: (String, a) } deriving (Show)

instance Monad Writer where
    return x = Writer ("", x)
    Writer (x, a) >>= k = let Writer (y, b) = k a
                          in Writer (x ++ y, b)

output :: String -> Writer ()
output s = Writer (s, ())

imprimirLinha :: Termo -> Int -> String
imprimirLinha t a = show t ++ " = " ++ show a ++ "\n"

--avaliar :: Termo -> Writer Int
--avaliar (Const a) = output (imprimirLinha (Const a) a) >>= \_ ->
--                    return a
--avaliar (Div t u) = avaliar t >>= \a ->
--                    avaliar u >>= \b ->
--                    output (imprimirLinha (Div t u) (a `div` b)) >>= \_ ->
--                    return (a `div` b)

avaliar :: Termo -> Writer Int
avaliar (Const a) = do output (imprimirLinha (Const a) a)
                       return a
avaliar (Div t u) = do a <- avaliar t
                       b <- avaliar u
                       output (imprimirLinha (Div t u) (a `div` b))
                       return (a `div` b)

expr = Div (Div (Const 1932) (Const 2)) (Const 23)
Writer (saida, resposta) = avaliar expr
