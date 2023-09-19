module Queue (makeQueue, empty, isEmpty, enqueue, dequeue, front, size, show) where

data Queue a = NuQu [a] [a]

makeQueue :: [a] -> Queue a
makeQueue l = NuQu [] l

empty :: Queue a
empty = NuQu [] []

isEmpty :: Queue a -> Bool
isEmpty (NuQu [] []) = True
isEmpty _ = False

enqueue :: a -> Queue a -> Queue a
enqueue x (NuQu ys zs) = NuQu ys (x:zs)

dequeue :: Queue a -> (a, Queue a)
dequeue (NuQu (x:xs) ys) = (x, NuQu xs ys)
dequeue (NuQu [] ys) = (z, NuQu zs [])
    where (z:zs) = reverse ys

front :: Queue a -> Maybe a
front (NuQu (x:_) _) = Just x
front (NuQu [] ys) = case reverse ys of
    [] -> Nothing
    (z:_) -> Just z

size :: Queue a -> Int
size (NuQu xs ys) = length xs + length ys

instance (Show a) => Show (Queue a) where
    show (NuQu xs ys) = show "{[" ++ printElems (xs ++ reverse ys) ++ "]}"

printElems :: (Show a) => [a] -> String
printElems [] = ""
printElems [x] = show x
printElems (x:xs) = show x ++ "->" ++ printElems xs
