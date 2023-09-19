data Queue a = NuQu [a] [a]

-- Construct an empty queue
empty :: Queue a
empty = NuQu [] []

-- Check if the queue is empty
isEmpty :: Queue a -> Bool
isEmpty (NuQu [] []) = True
isEmpty _ = False

-- Enqueue an element at the end
enqueue :: a -> Queue a -> Queue a
enqueue x (NuQu ys zs) = NuQu ys (x:zs)

-- Dequeue an element from the front
dequeue :: Queue a -> (Maybe a, Queue a)
dequeue (NuQu [] []) = (Nothing, NuQu [] [])
dequeue (NuQu (x:xs) ys) = (Just x, NuQu xs ys)
dequeue (NuQu [] ys) = dequeue (NuQu (reverse ys) [])

-- Retrieve the front element without dequeueing
front :: Queue a -> Maybe a
front (NuQu [] []) = Nothing
front (NuQu (x:_) _) = Just x
front (NuQu [] ys) = front (NuQu (reverse ys) [])

-- Convert a list to a queue
fromList :: [a] -> Queue a
fromList xs = NuQu xs []

-- Convert a queue to a list (in the order of dequeue)
toList :: Queue a -> [a]
toList q = reverse $ toListHelper q []
  where
    toListHelper (NuQu [] []) acc = acc
    toListHelper q'@(NuQu (x:xs) ys) acc = toListHelper (snd $ dequeue q') (x:acc)
    toListHelper q'@(NuQu [] ys) acc = toListHelper (NuQu (reverse ys) []) acc

-- Get the size of the queue
size :: Queue a -> Int
size (NuQu ys zs) = length ys + length zs
