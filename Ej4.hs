type STree a = Tree () a

data Tree t a = Zero t | Succ (Tree (Node t a) a)

data Node t a = Node2 t a t | Node3 t a t a t

-- Parte (b)

instance (Show t, Show a) => Show (Tree t a) where
  show = showTree

showTree :: (Show t, Show a) => Tree t a -> String
showTree (Zero nodes) = show nodes
showTree (Succ subtree) = showTree subtree

instance (Show t, Show a) => Show (Node t a) where
  show = showNode

showNode :: (Show t, Show a) => Node t a -> String
showNode (Node2 l value r) = "[ " ++ show l ++ " " ++ show value ++ " " ++ show r ++ " ]"
showNode (Node3 l v1 m v2 r) =
  "[ "
    ++ show l
    ++ " "
    ++ show v1
    ++ " "
    ++ show m
    ++ " "
    ++ show v2
    ++ " "
    ++ show r
    ++ " ]"

-- Parte (a)

a1 :: STree Integer
a1 = Succ $ Zero (Node2 () 2 ())

a2 :: STree Integer
a2 = Succ $ Succ $ Zero $ Node2 (Node2 () 2 ()) 20 (Node2 () 40 ())

a3 :: STree Integer
a3 =
  Succ $
    Succ $
      Succ $
        Zero $
          Node2
            (Node2 (Node2 () 1 ()) 10 (Node2 () 15 ()))
            20
            (Node3 (Node2 () 25 ()) 30 (Node2 () 35 ()) 40 (Node2 () 45 ()))

-- Parte (c1)

-- El problema con cant es que no se puede saber la cantidad de valores hasta evaluar los nodos, la cantidad
-- de Succ que aparecen puede dar una estimación, pero al tener la opcion de 1 o 2 valores dependiendo
-- si se utiliza Node2 o Node3 no es posible saber la cantidad exacta evaluando unicamente los (Tree t a).

-- Parte (c2)

class Cant t where
  cant :: t -> Int

instance Cant t => Cant (Tree t a) where
  cant = cantTree

cantTree :: Cant t => Tree t a -> Int
cantTree (Zero nodes) = cant nodes
cantTree (Succ subtree) = cantTree subtree

instance Cant t => Cant (Node t a) where
  cant = cantNode

cantNode :: Cant t => Node t a -> Int
cantNode (Node2 l _ r) = 1 + cant l + cant r
cantNode (Node3 l _ m _ r) = 2 + cant l + cant m + cant r

instance Cant () where
  cant () = 0
