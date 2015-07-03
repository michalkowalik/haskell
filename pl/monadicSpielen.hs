-- playing with monads

main = do
  -- Maybe wrapped in monad with >>= (bind)
  putStrLn $ show $ Just 9 >>= \x -> return (x * 10)

  -- the same with nothing -- should show nothing
  putStrLn $ show $ Nothing >>= \x -> return (x * 10)
