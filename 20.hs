rm xs n = ((xs !! (n-1)), (take (n-1) xs) ++ (drop n xs))
