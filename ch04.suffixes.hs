import Data.List(tails)

suffixes :: [a] -> [[a]]

suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []

suffixes5 = init . tails
