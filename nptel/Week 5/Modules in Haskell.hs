-- Modules

-- Queue.hs

module Queue (
    empty,
    isEmpty,
    enqueue,
    dequeue,
    makeQueue
) where

data Queue a = NuQu [a] [a]

makeQueue :: a -> Queue a
makeQueue l = NuQu l []
-- Now NuQu is not directly exposed


-- Function implementations

instance (Show a) => Show (Queue a) where
    show (NuQu xs ys) = "{[" ++ printElems (xs ++ reverse ys) ++ "]}"
    printElems :: (Show a) => [a] -> String
    printElems [] = ""
    printElems [x] = show x
    printElems (x:xs) = show x ++ "->" ++ printElems xs




