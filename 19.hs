rot xs nb = let h = take nb xs
                b = drop nb xs in
            b ++ h
