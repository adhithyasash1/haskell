-- In this example, the Color type is made an instance of the 
-- Show type class by providing a custom implementation of the show function. 
-- The show function is then used to convert a Color value to its string representation.

data Color = Red | Green | Blue

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"

main :: IO ()
main = do
    let color = Green
    putStrLn (show color)




-- In this example, the Book type is made an instance of the 
-- Show type class with a custom show implementation. The show 
-- function is then used to convert a Book value to its string representation.

data Book = Book String String

instance Show Book where
    show (Book title author) = "Title: " ++ title ++ ", Author: " ++ author

main :: IO ()
main = do
    let book = Book "The Catcher in the Rye" "J.D. Salinger"
    putStrLn (show book)




-- Enum Type

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

instance Show Day where
    show Monday = "Mon"
    show Tuesday = "Tue"
    show Wednesday = "Wed"
    show Thursday = "Thu"
    show Friday = "Fri"
    show Saturday = "Sat"
    show Sunday = "Sun"

main :: IO ()
main = do
    let today = Tuesday
    putStrLn ("Today is " ++ show today)




-- Custom data type with parameters

data Fruit = Apple String | Banana String

instance Show Fruit where
    show (Apple variety) = "Apple of variety: " ++ variety
    show (Banana country) = "Banana from: " ++ country

main :: IO ()
main = do
    let apple = Apple "Fuji"
        banana = Banana "Ecuador"
    putStrLn (show apple)
    putStrLn (show banana)




-- Tuple

data Point = Point Double Double

instance Show Point where
    show (Point x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

main :: IO ()
main = do
    let point = Point 3.0 4.0
    putStrLn ("Point coordinates: " ++ show point)




-- Compound data

data Person = Person String Int

instance Show Person where
    show (Person name age) = "Person {name: " ++ name ++ ", age: " ++ show age ++ "}"

main :: IO ()
main = do
    let person = Person "Alice" 25
    putStrLn (show person)
