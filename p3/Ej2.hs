module Ej2 where
  import Parser

  data Expr = Let VName Expr Expr
      | Add Expr Expr
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
                  e1 <- parserEXPR
                  pSym '+'
                  e2 <- parserEXPR
                  pSym ')'
                  return (Add e1 e2)
              <|>
              do  n <- num1
                  return (Num n) 
              <|>
              do  w <- word
                  return (Var w)