safeHead :: [a] -> Maybe a

safeFunc f [] = Nothing
safeFunc f xs = Just (f xs)


safeHead = safeFunc head


splitWith :: (a -> Bool) -> [a] -> [[a]]

splitWith _ [] = []
splitWith f x  =
  let (l, l') = break f x
      l''  = if null l' then [] else l'
  in l : splitWith f (dropWhile f l'')
