-- User defined data types :

data Bool = False | True
data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
data Shape = Circle Float | Square Float | Rectangle Float Float

weekend :: Day -> Bool
weekend Sat = True
weekend Sun = True
weekend _ = False

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Square r) = r * r
area (Rectangle l b) = l * b
  where
    pi = 3.14

data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a)

-- Example binary tree:   5
--                       / \
--                      3   8

exampleTree :: BinaryTree Int
exampleTree = Node 5 (Node 3 Leaf Leaf) (Node 8 Leaf Leaf)



-- Type Class and Deriving :

data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving Eq

data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving (Eq, Show, Ord)

data Point = Point Float Float
  deriving (Eq, Show)

pointA :: Point
pointA = Point 2.0 3.0

pointB :: Point
pointB = Point 2.0 3.0

equalPoints = pointA == pointB  -- Returns True
pointStr = show pointA          -- Returns "Point 2.0 3.0"



-- Constructors :

Sun :: Day
Rectangle :: Float -> Float -> Shape
Circle :: Float -> Shape

Circle 5.0 :: Shape
map Circle :: [Float] -> [Shape]
map Circle [3.0, 2.0] = [Circle 3.0, Circle 2.0]

data Color = Red | Green | Blue | Yellow

complementaryColor :: Color -> Color
complementaryColor Red = Green
complementaryColor Green = Red
complementaryColor Blue = Yellow
complementaryColor Yellow = Blue

colors :: [Color]
colors = [Red, Green, Blue, Yellow]

complementaryColors = map complementaryColor colors



-- Records :

data Person = Person String Int Float String
  deriving Show

name :: Person -> String
name (Person n _ _ _) = n

age :: Person -> Int
age (Person _ a _ _) = a

data Person = Person
  { name :: String
  , age :: Integer
  , height :: Float
  , num :: String
  }
  deriving Show

guy = Person { name = "sashi", age = 26, height = 5.8, num = "17" }

name guy  -- Returns "sashi"
age guy   -- Returns 26

data Person = Person
  { name :: String
  , age :: Int
  , height :: Float
  , num :: String
  }
  deriving Show

-- Creating a new person using record syntax
newPerson :: Person
newPerson = Person { name = "Alice", age = 30, height = 5.6, num = "123" }

-- Accessing fields using record syntax
personName :: String
personName = name newPerson

personAge :: Int
personAge = age newPerson



-- Type Parameters : Data types can also have type parameters, allowing you to create more generic structures

data List a = Empty | Cons a (List a) -- Here, List takes a type parameter a, and it can represent a list of any type

intList :: List Int
intList = Cons 1 (Cons 2 (Cons 3 Empty))

charList :: List Char
charList = Cons 'a' (Cons 'b' (Cons 'c' Empty))






