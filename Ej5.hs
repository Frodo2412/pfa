{-# OPTIONS_GHC -Wno-missing-methods #-}
-- EDSL
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import System.Console.Terminfo (Color)

{-# HLINT ignore "Use camelCase" #-}
class EDSL_Markup m where
  -- PartAB
  text :: String -> m
  bold :: m -> m
  italics :: m -> m
  underline :: m -> m
  url :: String -> m -> m
  size :: Int -> m -> m
  list :: [m] -> m
  (<->) :: m -> m -> m
  (<+>) :: m -> m -> m
  generate :: m -> String

  -- Part C
  generateB :: m -> Int

  -- Part D
  color :: Int -> Int -> Int -> m -> m

-- Shallow Embedding

type SMarkup = String

instance EDSL_Markup SMarkup where
  text m = m
  bold m = "<b>" ++ m ++ "</b>"
  italics m = "<i>" ++ m ++ "</i>"
  underline m = "<u>" ++ m ++ "</u>"
  url u m = "<a href=\"" ++ u ++ "\">" ++ m ++ "</a>"
  size i m = "<span style=\"font-size:" ++ show i ++ "px;\">" ++ m ++ "</span>"
  list m = "<ul>" ++ concatMap (\item -> "<li>" ++ item ++ "</li>") m ++ "</ul>"
  m1 <-> m2 = m1 ++ "<br/>" ++ m2
  m1 <+> m2 = m1 ++ " " ++ m2
  generate m = m
  color r g b m = "<span style=\"color:rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")\">" ++ m ++ "</span>"

type SLength = Int

-- Part B: Toda la clase tuvo que ser reimplementada para poder contar la cantidad de palabras
instance EDSL_Markup SLength where
  text txt = length $ words txt
  bold txt = txt
  italics txt = txt
  underline txt = txt
  url _ txt = txt
  size _ txt = txt
  list = sum
  (<->) = (+)
  (<+>) = (+)
  generate = show
  generateB = id

-- Deep Embedding

data DMarkup
  = Text String
  | Bold DMarkup
  | Italics DMarkup
  | Underline DMarkup
  | Url String DMarkup
  | Size Int DMarkup
  | List [DMarkup]
  | DMarkup :<-> DMarkup
  | DMarkup :<+> DMarkup
  | Color Int Int Int DMarkup

instance EDSL_Markup DMarkup where
  text = Text
  bold = Bold
  italics = Italics
  underline = Underline
  url = Url
  size = Size
  list = List
  m1 <-> m2 = m1 :<-> m2
  m1 <+> m2 = m1 :<+> m2
  color = Color
  generate (Text str) = str
  generate (Bold markup) = "<b>" ++ generate markup ++ "</b>"
  generate (Italics markup) = "<i>" ++ generate markup ++ "</i>"
  generate (Underline markup) = "<u>" ++ generate markup ++ "</u>"
  generate (Url url markup) = "<a href=\"" ++ url ++ "\">" ++ generate markup ++ "</a>"
  generate (Size size markup) =
    "<span style=\"font-size:"
      ++ show size
      ++ "px;\">"
      ++ generate markup
      ++ "</span>"
  generate (List items) =
    "<ul>"
      ++ concatMap (\item -> "<li>" ++ generate item ++ "</li>") items
      ++ "</ul>"
  generate (markup1 :<-> markup2) = generate markup1 ++ "<br/>" ++ generate markup2
  generate (markup1 :<+> markup2) = generate markup1 ++ " " ++ generate markup2
  generate (Color r g b markup) =
    "<span style=\"color:rgb("
      ++ show r
      ++ ","
      ++ show g
      ++ ","
      ++ show b
      ++ ")\">"
      ++ generate markup
      ++ "</span>"

  -- PartB: Simplemente se agrega un nuevo interpretexx que cuenta la cantidad de palabras
  generateB :: DMarkup -> Int
  generateB (Text str) = length $ words str
  generateB (Bold markup) = generateB markup
  generateB (Italics markup) = generateB markup
  generateB (Underline markup) = generateB markup
  generateB (Url _ markup) = generateB markup
  generateB (Size _ markup) = generateB markup
  generateB (List items) = sum $ map generateB items
  generateB (markup1 :<-> markup2) = generateB markup1 + generateB markup2
  generateB (markup1 :<+> markup2) = generateB markup1 + generateB markup2

-- Ejemplos
ex1 :: SMarkup
ex1 = text "hola soy un texto sin formato"

ex2 :: SMarkup
ex2 = bold (text "hola soy bold") <+> (underline . bold $ text "y subrayado")

ex3 :: SMarkup
ex3 =
  ex1
    <-> size 35 (ex2 <+> text "y grande")
    <-> text "consultar en:"
    <+> url "http://www.google.com" (bold $ text "Google")

ex4 :: SMarkup
ex4 =
  text "la lista es:"
    <+> list [ex1, ex2, italics $ text "y nada mas"]

ex5 :: SMarkup
ex5 =
  bold (text "Hola")
    <+> color 255 0 0 (text "soy un texto en rojo")
    <+> text "pero se me pasa."

ex1Length :: SLength
ex1Length = text "hola soy un texto sin formato"

ex2Length :: SLength
ex2Length = bold (text "hola soy bold") <+> (underline . bold $ text "y subrayado")

ex3Length :: SLength
ex3Length =
  ex1Length
    <-> size 35 (ex2Length <+> text "y grande")
    <-> text "consultar en:"
    <+> url "http://www.google.com" (bold $ text "Google")

ex4Length :: SLength
ex4Length =
  text "la lista es:"
    <+> list [ex1Length, ex2Length, italics $ text "y nada mas"]

main = do
  writeFile "out/shallowS/ex1.html" (generate ex1)
  writeFile "out/shallowS/ex2.html" (generate ex2)
  writeFile "out/shallowS/ex3.html" (generate ex3)
  writeFile "out/shallowS/ex4.html" (generate ex4)
  writeFile "out/shallowS/ex5.html" (generate ex5)
  print $ generateB ex1Length
  print $ generateB ex2Length
  print $ generateB ex3Length
  print $ generateB ex4Length

