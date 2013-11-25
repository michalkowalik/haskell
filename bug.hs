import Data.List
import System.Random

data Bug = Bug {x :: Integer, y :: Integer}
instance Eq Bug where
  (Bug x y) == (Bug x1 y1) = (x == x1) && (y == y1)

instance Show Bug where
  show (Bug x y) = "[x:" ++ show x ++ ", y:" ++ show y ++ "]"

-- board size:
size = 15 :: Integer

-- initial setting of bugs, one on each field.
bc = [(Bug x y) | x <- [0..(size - 1)], y <- [0..(size - 1)]]

-- generate board showing bugs 
board :: [Bug] -> [((Integer, Integer), Int)]
board bc = [((x,y), length $ filter (\a -> a == Bug x y) bc) | x <- [0..(size - 1)], y <- [0..(size - 1)]]

-- a simple view of a single board line:
--lineView :: Integer -> [((Integer, Integer), Int)] -> [Char]
--lineView line b = (unwords [show $ snd x | x <- b, (fst (fst x)) == line]) ++ "\n"

-- a simple view of a complete board:
--boardView :: [((Integer, Integer), Int)] -> [Char]
--boardView b = unwords [lineView x b | x <- [0..(size - 1)]]

boardView :: [((Integer, Integer), Int)] -> [Char]
boardView b = unwords [lineView (filter (\a -> (fst (fst a)) == x) b) | x <- [0..(size - 1)]]
               where lineView bb = (unwords [show $ snd x | x <- bb]) ++ "\n"

-- move bug:
move :: (Integer, Integer) -> Bug -> Bug
move (dx,dy) (Bug x y) = if (x + dx) < size && (y + dy) < size && (x + dx) >= 0 && (y + dy) >= 0 then
                           Bug (x + dx) (y + dy)
                         else
                           Bug x y

-- return a list of tuples with x,y deltas
-- works inside of IO monad!
deltas :: IO [(Integer, Integer)]
deltas = do
  g <- newStdGen
  g'<- newStdGen
  return $ zip (take (15^2) (randomRs (-1, 1) g :: [Integer]))
               (take (15^2) (randomRs (-1, 1) g':: [Integer]))

main :: IO ()
main = do
  putStrLn "bugs on the cheesboard, initial state:"
  putStrLn $ boardView $ board bc
  d <- deltas
  
  let b = board $ [move (d!!x) (bc!!x) | x <- [0..224]]
  putStrLn "Modified table:"
  --print b
  putStrLn $ boardView b

  putStrLn ("sum of bugs on the board: " ++  show (sum $ map snd b))
  putStrLn "Max value on the board: "
  print $ [x | x <- b, (snd x) == (maximum $ map snd b)]
--  putStrLn $ boardView b
