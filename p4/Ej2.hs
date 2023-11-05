fix :: (a -> a) -> a
fix f = f (fix f)

suma :: Int -> Int
suma = fix (\r n -> if n == 0 then 0 else n + r (n - 1))