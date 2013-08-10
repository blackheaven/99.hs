import Data.List

cb :: Int -> [a] -> [[a]]
cb 0 _ = [[]]
cb _ [] = []
cb n (x:xs) = (map (x:) (cb (n-1) xs)) ++ (cb n xs)

grp' :: (Eq a) => [Int] -> [a] -> [[[a]]] -> [[[a]]]
grp' [] _ rs = rs
grp' (n:ns) xs rs = let s = [ r ++ [x] | r <- rs, x <- cb n xs ] in grp' ns xs s

isValid :: (Eq a) => [a] -> Bool
isValid xs = (length xs) == (length $ nub xs)

grp :: (Eq a) => [Int] -> [a] -> [[[a]]]
grp gs xs = filter (notElem []) (grp' gs xs [[[]]])
