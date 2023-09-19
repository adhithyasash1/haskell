-- Abstract Data Types

-- Stacks

data Stack = Stack [Int]

empty :: Stack
empty = Stack []

push :: Int -> Stack -> Stack
push x (Stack xs) = Stack (x:xs)

pop :: Stack -> (Int, Stack)
pop (Stack (x:xs)) = (x, Stack xs)

isEmpty :: Stack -> Bool
isEmpty (Stack []) = True
isEmpty (Stack _) = False


-- Type Parameters

data Stack a = Stack [a]
  deriving (Eq, Show, Ord)

empty :: Stack a
empty = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (a, Stack a)
pop (Stack (x:xs)) = (x, Stack xs)

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty (Stack _) = False


-- Customizing Built-in Functions (Type Classes) :

printElems :: Show a => [a] -> String
printElems [] = ""
printElems [x] = show x
printElems (x:xs) = show x ++ "->" ++ printElems xs

instance Show a => Show (Stack a) where
  show (Stack l) = printElems l


-- Queues 

-- Naive Implementation

data Queue a = Queue [a]

empty :: Queue a
empty = Queue []

isEmpty :: Queue a -> Bool
isEmpty (Queue []) = True
isEmpty (Queue _) = False

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs) = Queue(xs + [x])

dequeue :: Queue a -> (a, Queue a)
dequeue Queue(x:xs) = (x, Queue xs)

-- Efficient Implementation

data Queue a = NuQu [a] [a]

enqueue :: a -> Queue a -> Queue a
enqueue x (NuQu ys zs) = NuQu ys (x:zs)

dequeue :: Queue a -> (a, Queue a)
dequeue (NuQu (x:xs) ys) = (x, NuQu xs ys)
dequeue (NuQu [] ys) = dequeue (NuQu (reverse ys) [])






