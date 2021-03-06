data Occ a = S a | M Int a deriving(Show)

inc (S x) = M 2 x
inc (M n x) = M (n + 1) x

eo :: Eq a => [a] -> [Occ a]
eo [] = []
eo (x:xs) = let f [] _ r t = t ++ [r]
                f (z:zs) m r t
                    | z == m = f zs z (inc r) t
                    | otherwise = f zs z (S z) (t ++ [r]) in
            f xs x (S x) []

dec [] = []
dec ((S a):xs) = a:dec xs
dec ((M n a):xs) = (replicate n a) ++ dec xs
