data Termo = Const Int | Div Termo Termo deriving (Show)

avaliar :: Termo -> Int
avaliar (Const n) = n
avaliar (Div t n) = avaliar t `div` avaliar n

expr = Div (Div (Const 1932) (Const 2))
           (Const 23)

divisaoPorZero = Div (Const 1) (Const 0)

resposta = avaliar expr
erro = avaliar divisaoPorZero
