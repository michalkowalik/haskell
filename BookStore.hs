-- chapter 03:

type CustomerId = Int
type ReviewBody = String

data BookInfo = Book Int String [String]
                deriving (Show)

bookID      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors

book1 = Book 12345 "Book Title"
        ["Author 1", "Author 2"]

data BookReview = BookReview BookInfo CustomerId String        


type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerId
                   deriving (Show)



-- various things:
type Vector =  (Double, Double)
data Shape = Circle Vector Double
           | Poly [Vector]

data Tree a  = Node a (Tree a) (Tree a)
             | Empty
               deriving(Show)
                       
--treeHeigth :: Node -> Int
treeHeigth tree =
  case tree of
    Empty -> 0
    (Node _ l r) -> 1 + max (treeHeigth l) (treeHeigth r)


--fromList (x : xs) = x : fromList(xs)
--fromList Nil = []

mySecond :: [a] -> a
mySecond xs = if null (tail xs)
              then error "list too short"
              else head (tail xs)


safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))


tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _ = Nothing

myLastButOne :: [a] -> Maybe a
myLastButOne (x : _ : []) = Just x
myLastButOne (_ : r) = myLastButOne r
myLastButOne _ = Nothing


pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
    where plural 0 = "no " ++ word ++ "s"
          plural 1 = "one " ++ word
          plural n = show n ++ " " ++ word ++ "s"


myCount :: [a] -> Int
myCount [] = 0
myCount (x:xs) = 1 + myCount xs

myCount2 x = helper 0 x
  where helper acc (x:xs) | acc >= 0 = helper (acc +1) xs
        helper acc [] = acc


myCount3 x = helper 0 x
  where helper acc (x:xs) = helper (acc +1) xs
        helper acc [] = acc
