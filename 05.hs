re [] = []
re (x:xs) = (re xs) ++ [x]
