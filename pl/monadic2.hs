import Data.Ratio
import Data.List
--import Data.List (all)

newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving Show

-- probe is functor, because list is a functor.
-- we need a cutom fmap implementation though:
instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

thisSituation :: Prob (Prob Char)
thisSituation = Prob
                [(Prob [('a', 1%2), ('b', 1%2)], 1%4),
                 (Prob [('c', 1%2), ('d', 1%2)], 3%4)
                 ]
                
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
  where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p * r)) innerxs

instance Monad Prob where
  return x = Prob [(x, 1%1)]
  m >>= f = flatten (fmap f m)
  fail _ = Prob []

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1%2), (Tails, 1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1%10), (Tails, 9%10)]

flipThree :: Prob Bool
flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (== Tails) [a, b, c])

red :: Prob Bool -> Prob Bool
red coins = Prob $ map (s . unzip) . groupBy (\a b -> fst a == fst b) . sort $ series
  where series = getProb coins
        s (key, value) = (head key, sum value)
