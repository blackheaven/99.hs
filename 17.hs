sp xs n = let f [] _ r = r
              f zs 0 (b, _) = (b, zs)
              f (z:zs) n (b, _) = f zs (n - 1) (b ++ [z], []) in
        f xs n ([], [])
