module Main where
    import Ej2
    import Parser
    import Control.Monad.Reader
    import System.Environment

    type Env = [(VName,Int)]

    -- Interprete (a)
    interp :: Expr -> Reader Env Int
    interp (Let var e1 e2) = do
        val <- interp e1
        local ((:) (var, val)) (interp e2)

    interp (Add e1 e2) = do
        val1 <- interp e1
        val2 <- interp e2
        return (val1 + val2)

    interp (Num n) = return n

    interp (Var var) = do
        env <- ask
        case lookup var env of
            Just val -> return val
            Nothing  -> error "Variable no encontrada en el ambiente"

    -- Evaluador (b)
    eval :: Expr -> Int
    eval expr = runReader (interp expr) []

    -- Main (c)
    main :: IO ()
    main = do
        args <- getArgs
        if null args 
            then putStrLn "No se proporcionaron argumentos" 
            else do
            let fileName = head args
            str <- readFile fileName
            let exp = fst $ head $ (runP parserEXPR) str
            let inter = eval exp
            print inter