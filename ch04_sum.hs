foldlSum xs = foldl step 0 xs
  where step acc x  = acc + x

niceSum :: [Integer] -> Integer
niceSum xs = foldl (+) 0 xs

-- Fold filter:

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p [] = []
myFilter p (x:xs)
  | p x = x : filter p xs
  | otherwise  = filter p xs
