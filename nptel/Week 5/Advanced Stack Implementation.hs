-- Advanced Stack Implementation : 

data LinkedList a = Empty | Node a (LinkedList a)
  deriving (Eq, Show)

data Stack a = Stack (LinkedList a)

empty :: Stack a
empty = Stack Empty

push :: a -> Stack a -> Stack a
push x (Stack ll) = Stack (Node x ll)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack Empty) = (Nothing, Stack Empty)
pop (Stack (Node x xs)) = (Just x, Stack xs)

isEmpty :: Stack a -> Bool
isEmpty (Stack Empty) = True
isEmpty (Stack _) = False

peek :: Stack a -> Maybe a -- Peeking at the Top Element 
peek (Stack Empty) = Nothing
peek (Stack (Node x _)) = Just x

fromList :: [a] -> Stack a -- Converting a List to a Stack using the foldl function
fromList = foldl (flip push) empty

toList :: Stack a -> [a] -- Converting a Stack to a List (recursive approach)
toList (Stack ll) = toList' ll
  where
    toList' Empty = []
    toList' (Node x xs) = x : toList' xs

reverseStack :: Stack a -> Stack a -- Reverse a Stack
reverseStack stack = fromList (toList stack)

combineStacks :: Stack a -> Stack a -> Stack a -- Combining Stacks
combineStacks stack1 stack2 = fromList (toList stack1 ++ toList stack2)



-- Implementing a Max Stack (A stack that keeps track of the maximum element)
-- The MaxStack implementation keeps track of the maximum element using an additional field. 
-- The pushMax and popMax functions ensure that the maximum element is always up-to-date.

data MaxStack a = MaxStack (Stack a) (Maybe a)

pushMax :: Ord a => a -> MaxStack a -> MaxStack a
pushMax x (MaxStack stack Nothing) = MaxStack (push x stack) (Just x)
pushMax x (MaxStack stack (Just maxElem)) = MaxStack (push x stack) (Just (max x maxElem))

popMax :: Ord a => MaxStack a -> (Maybe a, MaxStack a)
popMax (MaxStack stack Nothing) = (Nothing, MaxStack stack Nothing)
popMax (MaxStack stack (Just maxElem)) =
  let (top, newStack) = pop stack
      newMaxElem = if top == Just maxElem then maxInStack newStack else maxElem
  in (top, MaxStack newStack newMaxElem)

maxInStack :: Ord a => Stack a -> Maybe a
maxInStack (Stack ll) = maxInLinkedList ll

maxInLinkedList :: Ord a => LinkedList a -> Maybe a
maxInLinkedList Empty = Nothing
maxInLinkedList (Node x Empty) = Just x
maxInLinkedList (Node x rest) = maxMaybe x (maxInLinkedList rest)

maxMaybe :: Ord a => a -> Maybe a -> Maybe a
maxMaybe x Nothing = Just x
maxMaybe x (Just y) = Just (max x y)