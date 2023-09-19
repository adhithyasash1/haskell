-- Linked List :
-- Linked lists are linear data structures composed of nodes, where each node contains data and a reference to the next node.

data LinkedList a = Empty | Node a (LinkedList a)

-- Insert an element at the beginning
insertFront :: a -> LinkedList a -> LinkedList a
insertFront x xs = Node x xs

-- Delete the first occurrence of an element
delete :: Eq a => a -> LinkedList a -> LinkedList a
delete _ Empty = Empty
delete x (Node y rest)
    | x == y = rest
    | otherwise = Node y (delete x rest)

-- Convert a list to a linked list
listToLinkedList :: [a] -> LinkedList a
listToLinkedList [] = Empty
listToLinkedList (x:xs) = Node x (listToLinkedList xs)




-- Binary Search Tree (BST):
-- Binary search trees are hierarchical data structures where each node has at most two children, with left children being smaller and right children being larger.

data Tree a = EmptyTree | Node a (Tree a) (Tree a)

-- Insert an element into the BST
insert :: Ord a => a -> Tree a -> Tree a
insert x EmptyTree = Node x EmptyTree EmptyTree
insert x (Node val left right)
    | x == val = Node val left right
    | x < val = Node val (insert x left) right
    | x > val = Node val left (insert x right)

-- Search for an element in the BST
search :: Ord a => a -> Tree a -> Bool
search _ EmptyTree = False
search x (Node val left right)
    | x == val = True
    | x < val = search x left
    | x > val = search x right
```




-- Hash Map (Dictionary) :
-- Hash maps store key-value pairs, allowing efficient lookup and insertion based on keys.

data HashMap k v = HashMap [(k, v)]

-- Insert a key-value pair into the hash map
insert :: Eq k => k -> v -> HashMap k v -> HashMap k v
insert key value (HashMap kvs) = HashMap $ (key, value) : filter ((/= key) . fst) kvs

-- Lookup a value by key
lookup :: Eq k => k -> HashMap k v -> Maybe v
lookup key (HashMap kvs) = fmap snd $ find (\(k, _) -> k == key) kvs
```




-- Priority Queue (Min Heap):
-- Priority queues allow efficient retrieval of the minimum element.

data PriorityQueue a = EmptyPQ | PQNode a Int (PriorityQueue a) (PriorityQueue a)

-- Insert an element with a priority into the priority queue
insert :: Ord a => a -> Int -> PriorityQueue a -> PriorityQueue a
insert x p queue = merge queue (PQNode x p EmptyPQ EmptyPQ)

-- Merge two priority queues
merge :: Ord a => PriorityQueue a -> PriorityQueue a -> PriorityQueue a
merge EmptyPQ q = q
merge q EmptyPQ = q
merge pq1@(PQNode x p _ _) pq2@(PQNode y q _ _)
    | p <= q = joinPQ x p pq1 pq2
    | otherwise = joinPQ y q pq2 pq1

joinPQ :: a -> Int -> PriorityQueue a -> PriorityQueue a -> PriorityQueue a
joinPQ x p EmptyPQ q2 = PQNode x p q2 EmptyPQ
joinPQ x p q1@(PQNode y q EmptyPQ _) q2
    | p <= q = PQNode x p q2 q1
    | otherwise = PQNode y q (joinPQ x p q EmptyPQ) q2
joinPQ x p q1 q2@(PQNode y q EmptyPQ _)
    | p <= q = PQNode x p q2 q1
    | otherwise = PQNode y q q2 (joinPQ x p EmptyPQ q)
