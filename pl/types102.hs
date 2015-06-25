import qualified Data.Map as Map

data Person = Person {firstName :: String,
                      lastName :: String,
                      age :: Int} deriving (Eq, Show, Read)

type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name phoneNumber phoneBook = (name, phoneNumber) `elem` phoneBook

type AssocList k v = [(k, v)]

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exit"
    Just(state, code) -> if state /= Taken
                         then Right code
                         else Left $ "Locker " ++ show lockerNumber ++ " taken"

lockers :: LockerMap  
lockers = Map.fromList   
          [(100,(Taken,"ZD39I"))  
          ,(101,(Free,"JAH3I"))  
          ,(103,(Free,"IQSA9"))  
          ,(105,(Free,"QOTSA"))  
          ,(109,(Taken,"893JJ"))  
          ,(110,(Taken,"99292"))  
          ]
                           
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


-- typeclass 102
data TrafficLights = Red | Yellow | Green

instance Eq TrafficLights where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLights where
  show Red = "Red light"
  show Green = "Green light"
  show Yellow = "Yellow light"

