module Stack (Stack(), empty, push, pop, isEmpty, top, size, show) where

data Stack a = Stack [a]

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (a, Stack a)
pop (Stack (x:xs)) = (x, Stack xs)

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty (Stack _) = False

top :: Stack a -> Maybe a
top (Stack []) = Nothing
top (Stack (x:_)) = Just x

size :: Stack a -> Int
size (Stack xs) = length xs

instance (Show a) => Show (Stack a) where
    show (Stack l) = printElems l

printElems :: (Show a) => [a] -> String
printElems [] = ""
printElems [x] = show x
printElems (x:xs) = show x ++ "->" ++ printElems xs
