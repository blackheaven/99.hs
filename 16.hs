dropEN [] _ = []
dropEN xs n = let f [] _ = []
                  f (z:zs) 1 = f zs n
                  f (z:zs) n = z:(f zs (n - 1)) in
            f xs n
