-- playing with monads
import Control.Monad


type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing
                                                 
-- this is pretty weird, but seems to work:
-- (yes, it's a function definition, the function is called "-:")
x -: f = f x

-- always fail:
banana :: Pole -> Maybe Pole
banana _ = Nothing

-- chess knight problem:
type KnightPos = (Int, Int)

-- get list of all possible knight moves starting from current
-- position:
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <- [(c+2, r+1), (c+2, r-1), (c-2, r+1), (c-2, r-1),
              (c+1, r+2), (c+1, r-2), (c-1, r+2), (c-1, r-2)]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c', r')

-- all position reachable in 3 moves:
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

main = do
  -- Maybe wrapped in monad with >>= (bind)
  putStrLn . show $ Just 9 >>= \x -> return (x * 10)

  -- the same with nothing -- should show nothing
  putStrLn . show $ Nothing >>= \x -> return (x * 10)

  -- land birds on the pole:
  -- won't work for the version returning Maybe Pole
  --  putStrLn . show $ (0, 0) -: landLeft 1 -: landLeft 2 -: landRight 3

  -- working version with >>= :
  putStrLn "\nLine walker - monadic edition."
  putStrLn . show $ return (0, 0) >>= landLeft 1 >>= landLeft 2 >>= landRight 3

  -- always fail when step on banana:
  putStrLn "\nShould always fail when step on banana:"
  putStrLn . show $ return (0, 0) >>= landLeft 1 >>= banana

  -- filter out number with 7 - monadic way:
  putStrLn "\nfilter numbers with 7:"
  putStrLn . show $ [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
  -- the same as list comprehesion:
  putStrLn . show $ [x | x <- [1..50], '7' `elem` show x]
                                       
