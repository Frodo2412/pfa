module Main where
    import Parser
    import Control.Monad.Reader
    import Control.Monad.Except
    import Control.Monad.Identity
    import System.Environment

    data Expr = Let VName Expr Expr
      | Add Expr Expr
      | Div Expr Expr
      | Num Int
      | Var VName
      deriving Show
    type VName = String

    -- Parser de Expresiones (a)
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

    -- Definición del intérprete (b)
    data Error = ErrorDivZero | ErrorUnbound VName
        deriving Show
    type Env = [(VName,Int)]

    interp :: (MonadReader Env m,MonadError Error m) => Expr -> m Int
    interp (Let var e1 e2) = do
        val <- interp e1
        local ((:) (var, val)) (interp e2)

    interp (Add e1 e2) = do
        val1 <- interp e1
        val2 <- interp e2
        return (val1 + val2)

    interp (Div e1 e2) = do
        val1 <- interp e1
        val2 <- interp e2
        if val2 == 0
            then throwError ErrorDivZero
            else return (val1 `div` val2)

    interp (Num n) = return n

    interp (Var var) = do
        env <- ask
        case lookup var env of
            Just val -> return val
            Nothing  -> throwError (ErrorUnbound var)

    -- Evaluador (c)
    type M a = ExceptT Error (ReaderT Env Identity) a

    evalE :: Expr -> Either Error Int
    evalE expr = runIdentity (runReaderT (runExceptT (interp expr :: M Int)) [])

    -- Main
    main :: IO ()
    main = do
        args <- getArgs
        if null args 
            then putStrLn "No se proporcionaron argumentos" 
            else do
            let fileName = head args
            str <- readFile fileName
            let exp = fst $ head $ (runP parserEXPR) str
            let result = evalE exp
            case result of
                Left error -> putStrLn $ show error
                Right value -> putStrLn $ show value