import Data.List
import System.Random

data Bug = Bug {x :: Int, y :: Int}
instance Eq Bug where
  (Bug x y) == (Bug x1 y1) = (x == x1) && (y == y1)

instance Show Bug where
  show (Bug x y) = "<x:" ++ show x ++ ", y:" ++ show y ++ ">"

-- board size:
size = 15 :: Int

-- initial setting of bugs, one on each field.
bc = [(Bug x y) | x <- [0..(size - 1)], y <- [0..(size - 1)]]

-- generate board showing bugs 
board :: [Bug] -> [((Int, Int), Int)]
board bc = [((x,y), length $ filter (\a -> a == Bug x y) bc) | x <- [0..(size - 1)], y <- [0..(size - 1)]]

-- a simple view of a complete board:
boardView :: [((Int, Int), Int)] -> [Char]
boardView b = unwords [lineView (filter (\a -> (fst (fst a)) == x) b) | x <- [0..(size - 1)]]
               where lineView bb = (unwords [show $ snd x | x <- bb]) ++ "\n"

-- move bug:
move :: (Int, Int) -> Bug -> Bug
move (dx,dy) (Bug x y) = if (x + dx) < size && (y + dy) < size && (x + dx) >= 0 && (y + dy) >= 0 then
                           Bug (x + dx) (y + dy)
                         else
                           Bug x y

-- return a list of tuples with x,y deltas
-- works inside of IO monad!
deltas :: IO [(Int, Int)]
deltas = do
  g <- newStdGen
  g'<- newStdGen
  return $ zip (take (size ^ 2) (randomRs (-1, 1) g :: [Int]))
               (take (size ^ 2) (randomRs (-1, 1) g':: [Int]))


nextState :: [Bug] -> [(Int, Int)] -> [Bug]
nextState b d =  [(move (d!!x) (b!!x)) | x <- [0..(size ^ 2 - 1)]]

nextState' :: [Bug] -> IO [Bug]
nextState' bc = do
  d <- deltas
  return [(move (d!!x) (bc!!x)) | x <- [0..(size ^ 2 - 1)]]

 ---
 --  revs <- take 100 $ iterate nextState' bc
 ----
revs :: [Bug] -> IO [[Bug]]
revs bugs = do
  nextBugs <- nextState' bugs
  return $ bugs : nextBugs : []



main :: IO ()
main = do
  putStrLn "bugs on the cheesboard, initial state:"
  putStrLn $ boardView $ board bc
  d <- deltas

  let b  = board $ nextState  bc d
  bc' <- nextState' bc
  let b' = board $ bc'
  putStrLn "Modified table:"
  putStrLn $ boardView b'

  putStrLn ("sum of bugs on the board: " ++  show (sum $ map snd b))
  putStrLn "Max value on the board: "
  print $ [x | x <- b, (snd x) == (maximum $ map snd b)]
