import Data.Monoid
import qualified Data.Foldable as F

-- standard tree:

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left rigth)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x rigth


instance F.Foldable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend`
                           f x           `mappend`
                           F.foldMap f r


main = do
  let testTree = Node 5
                 (Node 3
                  (Node 1 EmptyTree EmptyTree)
                  (Node 6 EmptyTree EmptyTree))
                 (Node 9
                  (Node 8 EmptyTree EmptyTree)
                  (Node 10 EmptyTree EmptyTree))
  putStrLn . show $ F.foldl (+) 0 testTree
  putStrLn . show $ F.foldl (*) 1 testTree

  -- any:
  let any3 = getAny $ F.foldMap (\x -> Any $ x == 3) testTree
  putStrLn $ "if test tree contains any node with value3: " ++ show any3
 
  -- tree to list:
  let listFromTree = F.foldMap (\x -> [x]) testTree
  putStrLn $ show listFromTree
