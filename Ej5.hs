{-# OPTIONS_GHC -Wno-missing-methods #-}
-- EDSL
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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

-- Part C(A): Toda la clase tuvo que ser reimplementada para poder contar la cantidad de palabras
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

  -- PartC(B): Simplemente se agrega un nuevo interprete que cuenta la cantidad de palabras.
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
ex1Shallow :: SMarkup 
ex1Shallow = text "hola soy un texto sin formato"

ex2Shallow :: SMarkup
ex2Shallow = bold (text "hola soy bold") <+> (underline . bold $ text "y subrayado")

ex3Shallow :: SMarkup
ex3Shallow =
  ex1Shallow
    <-> size 35 (ex2Shallow <+> text "y grande")
    <-> text "consultar en:"
    <+> url "http://www.google.com" (bold $ text "Google")

ex4Shallow :: SMarkup
ex4Shallow =
  text "la lista es:"
    <+> list [ex1Shallow, ex2Shallow, italics $ text "y nada mas"]

ex5Shallow :: SMarkup
ex5Shallow =
  bold (text "Hola")
    <+> color 255 0 0 (text "soy un texto en rojo")
    <+> text "pero se me pasa."

ex1Deep :: DMarkup 
ex1Deep = text "hola soy un texto sin formato"

ex2Deep :: DMarkup
ex2Deep = bold (text "hola soy bold") <+> (underline . bold $ text "y subrayado")

ex3Deep :: DMarkup
ex3Deep =
  ex1Deep
    <-> size 35 (ex2Deep <+> text "y grande")
    <-> text "consultar en:"
    <+> url "http://www.google.com" (bold $ text "Google")

ex4Deep :: DMarkup
ex4Deep =
  text "la lista es:"
    <+> list [ex1Deep, ex2Deep, italics $ text "y nada mas"]

ex5Deep :: DMarkup
ex5Deep =
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
  writeFile "out/shallowS/ex1.html" $ generate ex1Shallow
  writeFile "out/shallowS/ex2.html" $ generate ex2Shallow
  writeFile "out/shallowS/ex3.html" $ generate ex3Shallow
  writeFile "out/shallowS/ex4.html" $ generate ex4Shallow
  writeFile "out/shallowS/ex5.html" $ generate ex5Shallow
  writeFile "out/deepD/ex1.html" $ generate ex1Deep
  writeFile "out/deepD/ex2.html" $ generate ex2Deep
  writeFile "out/deepD/ex3.html" $ generate ex3Deep
  writeFile "out/deepD/ex4.html" $ generate ex4Deep
  writeFile "out/deepD/ex5.html" $ generate ex5Deep
  print $ generateB ex1Length
  print $ generateB ex2Length
  print $ generateB ex3Length
  print $ generateB ex4Length

