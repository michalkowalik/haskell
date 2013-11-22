import System.Random
import Control.Monad (replicateM)

--main = replicateM 10 (randomIO :: IO Float) >>= print
main = do
  g <- newStdGen
  print $ take 10 (randomRs ('a','z') g)
