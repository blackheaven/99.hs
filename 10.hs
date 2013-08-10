ec :: Eq a => [a] -> [(Integer, a)]
ec [] = []
ec (x:xs) = let f [] _ r t = t ++ [r]
                f (z:zs) m r@(n, s) t
                    | z == m = f zs z (n + 1, s) t
                    | otherwise = f zs z (1, z) (t ++ [r]) in
            f xs x (1, x) []
