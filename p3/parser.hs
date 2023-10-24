module Parser where
    import Control.Monad
    import GHC.Base hiding ((<|>))
    import Data.Char hiding (isDigit)

    newtype Parser a = P {runP :: String -> [(a,String)]}

    instance Functor Parser where
        fmap f p = P $ \cs -> [(f a,cs') | (a,cs') <- runP p cs]

    instance Applicative Parser where
        pure a =  P (\cs -> [(a,cs)])
    -- (<*>) ::  Parser (a -> b) -> Parser a -> Parser b
        (P p) <*> (P q) = P $ \cs -> [ (f a, cs'')  |  (f , cs')   <- p cs
                                                ,  (a , cs'')  <- q cs']

    instance Monad Parser where
        (P p) >>= f = P $ \cs -> concat [runP (f a) cs' | (a,cs') <- p cs]

    -- | Parsers primitivos

    pFail :: Parser a
    pFail = P $ \cs -> []

    (<|>) :: Parser a -> Parser a -> Parser a
    (P p) <|> (P q) = P $ \cs -> case p cs ++ q cs of
                                []     -> []
                                (x:xs) -> [x]

    item :: Parser Char
    item = P $ \cs -> case cs of
                        ""     -> []
                        (c:cs) -> [(c,cs)]

    pSat :: (Char -> Bool) -> Parser Char
    pSat p = do c <- item
                if p c then return c
                    else pFail

    pSym :: Char -> Parser Char
    pSym c = pSat (== c)

    -- | reconocer un digito

    isDigit c = (c >= '0') && (c <= '9')

    digit :: Parser Int
    digit = do 
        c <- pSat isDigit
        return (ord c - ord '0')

    digits1 :: Parser [Int]
    digits1 = pList1 digit

    num1 = do 
        ds <- digits1
        return (foldl op 0 ds)
        where
            n `op` d = n*10 + d

    -- | recursion
    -- | cero o mas veces p

    pList :: Parser a -> Parser [a]
    pList p = do 
        a <- p
        as <- pList p
        return (a:as)
        <|>
        return []

    -- | una o mas veces p

    pList1 :: Parser a -> Parser [a]
    pList1 p = do 
        a <- p
        as <- pList p
        return (a:as)

    word :: Parser String
    word = pList1 (pSat p)
        where p c = isAlphaNum c

    space :: Parser String
    space = pList (pSat p)
        where p c = isSpace c