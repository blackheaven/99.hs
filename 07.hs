data NL a = E a | L [NL a]

fl (E a) = [a]
fl (L l) = let  f [] = []
                f [x] = fl x
                f (x:xs) = (fl x) ++ (f xs) in
            f l
