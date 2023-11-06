-- Curch Naturales
type Nat = forall a. (a -> a) -> a -> a

-- Representación del número cero
z :: Nat
z = \f x -> x

-- Función sucesor
s :: Nat -> Nat
s n = \f x -> f (n f x)

-- Natural 2
nat2 = s (s z)

-- Curch Nat a Entero
toInt :: Nat -> Int
toInt n = n (+1) 0

-- Fold para Nat
foldN :: forall t. (t -> t) -> t -> Nat -> t 
foldN h v n = n h v

-- Codificación del tipo par (a,b)
type Pair a b = forall z. (a -> b -> z) -> z

pair :: a -> b -> Pair a b
pair = \x y p -> p x y

-- (A) Intercambio de las componentes de un par
swap :: Pair a b -> Pair b a
swap p = \f -> p (\x y -> f y x)

-- (B) Duplicación de un Natural
dup :: Nat -> Nat
dup n = \f x -> foldN (nat2 f) x n

-- (C) Potencia de dos de un Natural
exp2 :: Nat -> Nat
exp2 n = n nat2