dd [] = []
dd (x:xs) = let f [] _ a = a
                f (x:xs) o a
                    | x == o = f xs x a
                    | otherwise = f xs x (a ++ [x]) in
            f xs x [x]
