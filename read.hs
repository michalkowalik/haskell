main = do
  putStrLn "enter a double:"
  inpStr <- getLine
  let inpDouble = (read inpStr)::Double
  putStrLn ("foo " ++ show inpDouble  ++ " is " ++ show (inpDouble * 2))
