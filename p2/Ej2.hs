data Tree a = Leaf a | Bin (Tree a) (Tree a)
  deriving (Show)

unfoldTree :: (b -> Either a (b, b)) -> b -> Tree a
unfoldTree next b = case next b of
  Left x -> Leaf x
  Right (l, r) -> Bin (unfoldTree next l) (unfoldTree next r)

replicateTree :: Int -> b -> Tree b
replicateTree n x = unfoldTree (\n -> if n == 0 then Left x else Right (n - 1, n - 1)) n

fromToTree :: Int -> Int -> Tree Int
fromToTree b e =
  unfoldTree
    ( \(v, t) ->
        if v == t
          then Left v
          else
            Right
              ( (v, if v > t then v - div (v - t) 2 else div (t - v) 2 + v),
                (if v > t then v - 1 - div (v - t) 2 else div (t - v) 2 + v + 1, t)
              )
    )
    (b, e)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = unfoldTree fTree
  where
    fTree (Leaf x) = Left (f x)
    fTree (Bin x y) = Right (x, y)