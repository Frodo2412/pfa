module Ej4 where
    import Parser

    data Expr = Let VName Expr Expr
      | Add Expr Expr
      | Div Expr Expr
      | Num Int
      | Var VName
      deriving Show
    type VName = String

    -- Parser de Expresiones
    parserEXPR :: Parser Expr
    parserEXPR = do pSym 'l'
                    pSym 'e'
                    pSym 't'
                    var <- word
                    pSym '='
                    ass <- parserEXPR
                    pSym 'i'
                    pSym 'n'
                    app <- parserEXPR
                    return (Let var ass app)
                <|>
                do  pSym '('
                    e1add <- parserEXPR
                    pSym '+'
                    e2add <- parserEXPR
                    pSym ')'
                    return (Add e1add e2add)
                <|>
                do  pSym '('
                    e1div <- parserEXPR
                    pSym '/'
                    e2div <- parserEXPR
                    pSym ')'
                    return (Div e1div e2div)
                <|>
                do  n <- num1
                    return (Num n) 
                <|>
                do  w <- word
                    return (Var w)