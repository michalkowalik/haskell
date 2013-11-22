import Data.Char(toUpper)

main = do interact ((++) "your data in uppercase: \n" .
                   map toUpper)
