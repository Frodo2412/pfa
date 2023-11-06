{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

data Empty

data NonEmpty

data SafeList a b where
  NilS :: SafeList a Empty
  ConsS :: a -> SafeList a b -> SafeList a NonEmpty

-- a

safeTail :: SafeList a NonEmpty -> Either (SafeList a Empty) (SafeList a NonEmpty)
safeTail (ConsS x xs) = case xs of
  NilS -> Left xs
  ConsS _ _ -> Right xs

-- b
safeLast :: SafeList a NonEmpty -> a
safeLast (ConsS x xs) = case xs of
  NilS -> x
  ConsS _ _ -> safeLast xs

-- c

data Zero

data Succ a

data Vec a n where
  Nil :: Vec a Zero
  Cons :: a -> Vec a n -> Vec a (Succ n)

data Ret n where
    IsEmpty :: Zero -> Ret Empty
    IsNotEmpty :: Succ n -> Ret NonEmpty 

vec2safe :: Vec a n -> SafeList a (Ret n)
vec2safe Nil = NilS
vec2safe (Cons x xs) = ConsS x (vec2safe xs)
