gg :: Eq a => [a] -> [[a]]
gg [] = []
gg (x:xs) = let f [] _ r t = t ++ [r]
                f (z:zs) m r t
                    | z == m = f zs z (z:r) t
                    | otherwise = f zs z [z] (t ++ [r]) in
            f xs x [x] []
