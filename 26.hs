import Data.List
disp :: (Eq a) => [a] -> Int -> [[a]] -> [[a]]
disp _ 0 r = filter (\x -> (length x) == (length $ nub x)) r
disp xs n r = disp xs (n - 1) [a:b | a <- xs, b <- r]

cb 0 _ = []
cb _ [] = []
cb n xs = disp xs n [[]]
