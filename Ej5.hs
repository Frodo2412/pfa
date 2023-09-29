-- EDSL
class EDSL_Markup m where
  text      :: String -> m
  bold      :: m -> m
  italics   :: m -> m
  underline :: m -> m
  url       :: String -> m -> m
  size      :: Int -> m -> m
  list      :: [m] -> m
  (<->)     :: m -> m -> m
  (<+>)     :: m -> m -> m
  generate  :: m -> String

-- Shadow Embedding

type SMarkup = String

instance EDSL_Markup SMarkup where
        text m      = m
        bold m      = "<b>" ++ m ++ "</b>"
        italics m   = "<i>" ++ m ++ "</i>"
        underline m = "<u>" ++ m ++ "</u>"
        url u m     = "<a href=\"" ++ u ++ "\">" ++ m ++ "</a>"
        size i m    = "<span style=\"font-size:" ++ show i ++ "px;\">" ++ m ++ "</span>"
        list m      = "<ul>" ++ concatMap (\item -> "<li>" ++ item ++ "</li>") m ++  "</ul>"
        m1 <-> m2   = m1 ++ "<br/>" ++ m2
        m1 <+> m2   = m1 ++ " " ++ m2
        generate m  = m

-- Deep Embedding

data DMarkup = Text String
            | Bold DMarkup
            | Italics DMarkup
            | Underline DMarkup
            | Url String DMarkup
            | Size Int DMarkup
            | List [DMarkup]
            | DMarkup :<-> DMarkup
            | DMarkup :<+> DMarkup

-- Directivas

instance EDSL_Markup DMarkup where
        text str = Text str
        bold m = Bold m
        italics i = Italics i
        underline u = Underline u
        url u m = Url u m
        size s m = Size s m
        list m = List m
        m1 <-> m2 = m1 :<-> m2
        m1 <+> m2 = m1 :<+> m2
        generate (Text str)             = str
        generate (Bold markup)          = "<b>" ++ generate markup ++ "</b>"
        generate (Italics markup)       = "<i>" ++ generate markup ++ "</i>"
        generate (Underline markup)     = "<u>" ++ generate markup ++ "</u>"
        generate (Url url markup)       = "<a href=\"" ++ url ++ "\">" ++ generate markup ++ "</a>"
        generate (Size size markup)     = "<span style=\"font-size:" ++ show size ++ "px;\">" 
                                        ++ generate markup ++ "</span>"
        generate (List items)           = "<ul>" ++ 
                                        concatMap (\item -> "<li>" ++ generate item ++ "</li>") items 
                                        ++  "</ul>"
        generate (markup1 :<-> markup2) = generate markup1 ++ "<br/>" ++ generate markup2
        generate (markup1 :<+> markup2) = generate markup1 ++ " " ++ generate markup2



-- Ejemplos
ex1 :: SMarkup
ex1 = text "hola soy un texto sin formato"

ex2 :: SMarkup
ex2 = (bold $ text "hola soy bold") <+> (underline . bold $ text "y subrayado")

ex3 :: SMarkup
ex3 = ex1 
        <-> size 35 (ex2 <+> text "y grande") 
        <-> text "consultar en:"
            <+>
            url "http://www.google.com" (bold $ text "Google")

ex4 :: SMarkup
ex4 = text "la lista es:"
        <+>
        list [ex1,ex2,italics $ text "y nada mas"]

main = do 
    writeFile "ex1.html" (generate ex1)
    writeFile "ex2.html" (generate ex2)
    writeFile "ex3.html" (generate ex3)
    writeFile "ex4.html" (generate ex4)