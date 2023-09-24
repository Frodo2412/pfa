-- takeWhile :: (a -> Bool) -> [a] -> [a]
-- takeWhile p [] = []
-- takeWhile p (x:xs)  | p x       = x : takeWhile p xs
--                     | otherwise = []

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith f [] ys = []
-- zipWith f (x:xs) [] = []
-- zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- tails :: [a] -> [[a]]
-- tails [] = [[]]
-- tails ys@(x:xs) = ys : tails xs

-- evens :: [a] -> [a]
-- evens [] = []
-- evens [x] = []
-- evens (x:y:xs) = y : evens xs

unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr next s = case next s of
  Nothing -> []
  Just (x, r) -> x : unfoldr next r

takeWhileU :: (a -> Bool) -> [a] -> [a]
takeWhileU p = unfoldr tWhile
  where
    tWhile [] = Nothing
    tWhile (y : ys) = if (p y) then Just (y, ys) else Nothing

zipWithU :: (a -> b -> c) -> ([a], [b]) -> [c]
zipWithU f = unfoldr zWith
  where
    zWith ([], _) = Nothing
    zWith (_, []) = Nothing
    zWith ((x : xs), (y : ys)) = Just (f x y, (xs, ys))

tailsU :: [a] -> [[a]]
tailsU x = unfoldr tU (Just (x, Nothing))
  where
    tU Nothing = Nothing
    tU (Just ([], _)) = Just ([], Nothing)
    tU (Just (ys@(x : xs), _)) = Just (ys, Just (xs, Nothing))

evensU :: [a] -> [a]
evensU = unfoldr eU
  where
    eU [] = Nothing
    eU [x] = Nothing
    eU (x : y : xs) = Just (y, xs)

-- Rerp

data Repr a = forall s. Repr (s -> Maybe (a, s)) s

toRepr :: [a] -> Repr a
toRepr xs = Repr next xs
  where
    next [] = Nothing
    next (x : xs) = Just (x, xs)

fromRepr :: Repr a -> [a]
fromRepr (Repr next s) = unfoldr next s

takeWhileR :: (a -> Bool) -> Repr a -> Repr a
takeWhileR p (Repr next s) = Repr next' s
  where
    next' s = case next s of
      Nothing -> Nothing
      Just (x, r) -> if p x then Just (x, r) else Nothing

zipWithR :: (a -> b -> c) -> Repr a -> Repr b -> Repr c
zipWithR f (Repr next1 s1) (Repr next2 s2) = Repr next' (s1, s2)
  where
    next' (sa, sb) = case (next1 sa, next2 sb) of
      (Nothing, _) -> Nothing
      (_, Nothing) -> Nothing
      (Just (x, r1), Just (y, r2)) -> Just (f x y, (r1, r2))

tailsR :: Repr a -> Repr (Repr a)
tailsR (Repr next s) = Repr next' s
  where
    next' s = case next s of
      Nothing -> Just (Repr (const Nothing) s, s)
      Just (x, r) -> Just (Repr next r, r)

evensR :: Repr a -> Repr a
evensR (Repr next s) = Repr next' s
  where
    next' s = case next s of
      Nothing -> Nothing
      Just (x, r) -> case next r of
        Nothing -> Nothing
        Just (y, r') -> Just (y, r')

-- Streams

data Stream a = forall s. Stream (s -> Step a s) s

data Step a s
  = Done -- corresponde a Nothing
  | Yield a s -- corresponde a Just
  | Skip s -- transicion silenciosa

stream :: [a] -> Stream a
stream xs = Stream next xs
  where
    next [] = Done
    next (x : xs) = Yield x xs

unstream :: Stream a -> [a]
unstream (Stream next s) = unfold s
  where
    unfold s = case next s of
      Done -> []
      Yield a s' -> a : unfold s'
      Skip s' -> unfold s'

takeWhileS :: (a -> Bool) -> Stream a -> Stream a
takeWhileS p (Stream next s) = Stream next' s
  where
    next' s = case next s of
      Done -> Done
      Yield a s' -> if p a then Yield a s' else Done
      Skip s' -> Skip s'

zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f (Stream next1 s1) (Stream next2 s2) = Stream next' (s1, s2)
  where
    next' (sa, sb) = case (next1 sa, next2 sb) of
      (Done, _) -> Done
      (_, Done) -> Done
      (Yield a s1', Yield b s2') -> Yield (f a b) (s1', s2')
      (Skip s1', _) -> Skip (s1', sb)
      (_, Skip s2') -> Skip (sa, s2')

tailsS :: Stream a -> Stream (Stream a)
tailsS (Stream next s) = Stream next' s
  where
    next' s = case next s of
      Done -> Yield (Stream (const Done) s) s
      Yield a s' -> Yield (Stream next s') s'
      Skip s' -> Skip s'

evensS :: Stream a -> Stream a
evensS (Stream next s) = Stream next' s
  where
    next' s = case next s of
      Done -> Done
      Yield a s' -> case next s' of
        Done -> Done
        Yield b s'' -> Yield b s''
        Skip s'' -> Skip s''
      Skip s' -> Skip s'
