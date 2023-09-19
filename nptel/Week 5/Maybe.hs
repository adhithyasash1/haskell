-- Maybe :


-- Example 1: max and printmax Functions

module MaxModule (maxValue, printMax) where

import Data.List (foldr)

maxValue :: (Ord a) => [a] -> Maybe a
maxValue [] = Nothing
maxValue xs = Just $ foldr max (head xs) xs

printMax :: (Show a, Ord a) => [a] -> String
printMax xs = case maxValue xs of
    Nothing -> "Empty list"
    Just x -> "Maximum = " ++ show x




-- Example 2: Table and myLookup Functions

module TableModule (Table, myLookup) where

type Key = Int
type Value = String
type Table = [(Key, Value)]

myLookup :: Key -> Table -> Maybe Value
myLookup _ [] = Nothing
myLookup k ((kl,v1):kvs)
    | k == kl = Just v1
    | otherwise = myLookup k kvs




-- Example 3: Additional Functionality for myLookup

import TableModule (myLookup)

insertIntoTable :: Key -> Value -> Table -> Table
insertIntoTable k v tbl = (k, v) : tbl

removeFromTable :: Key -> Table -> Table
removeFromTable _ [] = []
removeFromTable k ((kl,v1):kvs)
    | k == kl = kvs
    | otherwise = (kl, v1) : removeFromTable k kvs

updateTable :: Key -> Value -> Table -> Table
updateTable k v tbl = case myLookup k tbl of
    Just _ -> insertIntoTable k v $ removeFromTable k tbl
    Nothing -> tbl

main :: IO ()
main = do
    let initialTable = []
        updatedTable = insertIntoTable 1 "Value1" initialTable
        finalTable = updateTable 1 "NewValue" updatedTable
    putStrLn $ "Updated Table: " ++ show finalTable




-- Example 4: Handling Division
-- In this example, the safeDiv function performs division 
-- while handling the case of division by zero using the Maybe type. 
-- The main function demonstrates how to use safeDiv and 
-- handles the result using a case expression.

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

main :: IO ()
main = do
    putStrLn "Enter two numbers:"
    num1 <- readLn
    num2 <- readLn
    case safeDiv num1 num2 of
        Nothing -> putStrLn "Division by zero!"
        Just result -> putStrLn $ "Division result: " ++ show result




-- Example 5: Safe Head and Tail
-- In this example, the safeHead and safeTail functions provide 
-- safe versions of the head and tail functions for lists. 
-- The main function reads a list of numbers and demonstrates how 
-- to handle different cases using pattern matching within the case expression.

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

main :: IO ()
main = do
    putStrLn "Enter a list of numbers:"
    inputList <- readLn :: IO [Int]
    case (safeHead inputList, safeTail inputList) of
        (Nothing, _) -> putStrLn "List is empty!"
        (_, Nothing) -> putStrLn "List has only one element!"
        (Just first, Just rest) -> putStrLn $ "First element: " ++ show first ++ "\nRest of the list: " ++ show rest




-- Example 6: Handling Non-Negative Integers
-- In this example, the validatePositive function checks if an integer 
-- is non-negative and returns a Maybe Int result. 
-- The main function demonstrates how to use this function to validate user input.

validatePositive :: Int -> Maybe Int
validatePositive x
    | x >= 0 = Just x
    | otherwise = Nothing

main :: IO ()
main = do
    putStrLn "Enter a positive integer:"
    num <- readLn
    case validatePositive num of
        Nothing -> putStrLn "Invalid input! Please enter a positive integer."
        Just validNum -> putStrLn $ "You entered: " ++ show validNum