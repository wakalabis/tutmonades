data Termo = Const Int | Div Termo Termo deriving (Show)
data M a = Saida (String, a) deriving (Show)

avaliar :: Termo -> M Int
avaliar (Const a) = Saida (imprimirLinha (Const a) a, a)
avaliar (Div t u) = let Saida (x, a) = avaliar t
                        Saida (y, b) = avaliar u
                    in Saida (x ++ y ++ imprimirLinha (Div t u) (a `div` b), a `div` b)

imprimirLinha :: Termo -> Int -> String
imprimirLinha t a = show t ++ " = " ++ show a ++ "\n"


expr = Div (Div (Const 1932) (Const 2))
           (Const 23)

Saida (saida, resposta) = avaliar expr
