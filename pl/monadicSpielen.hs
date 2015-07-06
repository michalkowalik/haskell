-- playing with monads

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

main = do
  -- Maybe wrapped in monad with >>= (bind)
  putStrLn . show $ Just 9 >>= \x -> return (x * 10)

  -- the same with nothing -- should show nothing
  putStrLn . show $ Nothing >>= \x -> return (x * 10)

  -- land birds on the pole:
  -- won't work for the version returning Maybe Pole
  --  putStrLn . show $ (0, 0) -: landLeft 1 -: landLeft 2 -: landRight 3

  -- working version with >>= :
  putStrLn . show $ return (0, 0) >>= landLeft 1 >>= landLeft 2 >>= landRight 3
