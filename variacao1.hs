data Termo = Const Int | Div Termo Termo deriving (Show)
data M a = Valor a | Exc String deriving (Show)

avaliar :: Termo -> M Int
avaliar (Const n) = Valor n
avaliar (Div t u) = case avaliar t of
                       Exc e   -> Exc e 
                       Valor a -> case avaliar u of
                          Exc e   -> Exc e
                          Valor b -> if b == 0
                                     then Exc "divisao por zero"
                                     else Valor (a `div` b)
                    
expr = Div (Div (Const 1932) (Const 2))
           (Const 23)

divisaoPorZero = Div (Const 1) (Const 0)

resposta = avaliar expr
erro = avaliar divisaoPorZero
