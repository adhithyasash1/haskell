-- Define a recursive data type 'List' for lists
data List a = Nil | Cons a (List a)

-- 'Nil' represents an empty list, 'Cons' represents a list node with a value and a reference to the next node

-- Get the head of a 'List'
head :: List a -> Maybe a
head Nil = Nothing
head (Cons x _) = Just x

-- Example usage:
-- Creating 'List' instances
let emptyList = Nil
let myList = Cons 1 (Cons 2 (Cons 3 Nil))

-- Getting the head of a 'List'
head emptyList  -- Nothing
head myList     -- Just 1
