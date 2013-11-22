import Data.Char(digitToInt)

asInt_foldl :: String -> Int

asInt_foldl ('-' : xs) = negate(asInt_foldl xs)
asInt_foldl s = foldl (\a x -> a*10 + digitToInt x) 0 s
