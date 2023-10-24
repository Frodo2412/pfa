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
                  space
                  var <- word
                  space
                  pSym '='
                  space
                  ass <- parserEXPR
                  space
                  pSym 'i'
                  pSym 'n'
                  space
                  app <- parserEXPR
                  return (Let var ass app)
              <|>
              do  pSym '('
                  space
                  e1 <- parserEXPR
                  space
                  pSym '+'
                  space
                  e2 <- parserEXPR
                  space
                  pSym ')'
                  return (Add e1 e2)
              <|>
              do  n <- num1
                  return (Num n) 
              <|>
              do  w <- word
                  return (Var w)