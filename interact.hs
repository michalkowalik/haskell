import System.Environment (getArgs)
import SplitLines(splitLines, fixLines)

interactWith function inFile outFile = do
  input <- readFile inFile
  writeFile outFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"
        myFunction = fixLines
        
