import System.Environment (getArgs)

getFirstWords inFile = do
  input <- readFile inFile
  print (map (\x -> head (take 1 (words x))) (lines input))

main = mainWith
  where mainWith = do
          args <- getArgs
          case args of
            [f1] -> getFirstWords f1
            _ -> putStrLn "error: exactly one argument needed"
