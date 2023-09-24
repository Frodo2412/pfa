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

unfoldr :: (s -> Maybe (a,s)) -> s -> [a]
unfoldr next s = case next s of
                        Nothing    -> []
                        Just (x,r) -> x : unfoldr next r

takeWhileU :: (a -> Bool) -> [a] -> [a] 
takeWhileU p = unfoldr tWhile
    where tWhile []     = Nothing
          tWhile (y:ys) = if (p y) then Just (y,ys) else Nothing

zipWithU :: (a -> b -> c) -> ([a],[b]) -> [c]
zipWithU f = unfoldr zWith
    where zWith ([],_)          = Nothing
          zWith (_,[])          = Nothing 
          zWith ((x:xs),(y:ys)) = Just (f x y,(xs,ys))

tailsU :: [a] -> [[a]]
tailsU x = unfoldr tU (Just (x,Nothing))
    where tU Nothing              = Nothing
          tU (Just ([],_))        = Just ([],Nothing)
          tU (Just (ys@(x:xs),_)) = Just (ys,Just(xs,Nothing))

evensU :: [a] -> [a]
evensU = unfoldr eU
    where eU []       = Nothing
          eU [x]      = Nothing
          eU (x:y:xs) = Just (y,xs)
