my_reverse xs = foldl revOp [] xs
  where revOp acc x  = x : acc

my_rev2 xs = foldl (\acc x -> x : acc) [] xs
