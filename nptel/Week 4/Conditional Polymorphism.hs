-- Filter a list to only keep elements that satisfy a certain condition
-- In this example, the type constraint (a -> Bool) ensures that the function works with any type a for which a boolean condition can be checked.

filterExample :: (a -> Bool) -> [a] -> [a]
filterExample _ [] = []
filterExample p (x:xs)
  | p x       = x : filterExample p xs
  | otherwise = filterExample p xs


-- Finding the Sum of Elements
-- The (Num a) constraint ensures that the function works with any numeric type a.

sumExample :: (Num a) => [a] -> a
sumExample [] = 0
sumExample (x:xs) = x + sumExample xs


-- Finding Maximum element in a list
-- The (Ord a) constraint ensures that the function works with any type a for which comparison operations are defined.

maximumExample :: (Ord a) => [a] -> a
maximumExample [x] = x
maximumExample (x:xs) = max x (maximumExample xs)


-- These examples highlight how conditional polymorphism through type classes allows 
-- you to write generic functions that work with a wide range of types, as long as the 
-- types satisfy the required properties defined by the type class.




