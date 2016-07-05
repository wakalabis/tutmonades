data Termo = Const Int | Div Termo Termo deriving (Show)
data M a = Estado { obterEstado :: (Int -> (a, Int)) }

avaliar :: Termo -> M Int
avaliar (Const a) = Estado (\x -> (a, x))
avaliar (Div t u) = Estado $ \x -> let (a, y) = obterEstado (avaliar t) x
                                       (b, z) = obterEstado (avaliar u) y
                                   in (a `div` b, z + 1)

expr = Div (Div (Const 1932) (Const 2))
           (Const 23)

resposta = obterEstado (avaliar expr) 0
