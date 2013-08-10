import System.Random

rnd xs n = let f [] _ _ = []
               f _ _ 0 = []
               f xs r n = c xs (random r) n
               c xs (i, r) n = (xs !! p):f ((take (p-1)) ++ (drop p)) r (n - 1)
                where p = mod i (length xs) in
            f xs (mkStdGen 100) n
