-- Define a Set data structure
data Set a = Set [a]

-- Search for an element in the set
search :: Eq a => a -> Set a -> Bool
search x (Set y) = elem x y

-- Insert an element into the set
insert :: Eq a => a -> Set a -> Set a
insert x (Set y)
    | elem x y = Set y   -- If the element already exists, no change
    | otherwise = Set (x:y)  -- Otherwise, add the element to the list

-- Delete an element from the set
delete :: Eq a => a -> Set a -> Set a
delete x (Set y) = Set (filter (/=x) y)
-- Filter out elements that are not equal to x

-- Define a binary search tree data structure
data STree a = Nil | Node (STree a) a (STree a)
    deriving (Eq, Ord, Show)

-- Check if a tree is a valid binary search tree
isstree :: Ord a => STree a -> Bool
isstree Nil = True
isstree (Node t1 x t2) =
    isstree t1 && isstree t2 &&
    (t1 == Nil || maxt t1 < x) &&  -- Check left subtree
    (t2 == Nil || x < mint t2)  -- Check right subtree

-- Find the maximum value in a binary search tree
maxt :: Ord a => STree a -> a
maxt (Node t1 x t2) = max x (max y z)
    where
        y = if t1 == Nil then x else maxt t1
        z = if t2 == Nil then x else maxt t2

-- Find the minimum value in a binary search tree
mint :: Ord a => STree a -> a
mint (Node t1 x t2) = min x (min y z)
    where
        y = if t1 == Nil then x else mint t1
        z = if t2 == Nil then x else mint t2

-- Search for a value in a binary search tree
search :: Ord a => STree a -> a -> Bool
search Nil _ = False
search (Node tl x tr) v
    | x == v = True
    | v < x = search tl v
    | otherwise = search tr v

-- Insert a value into a binary search tree
insert :: Ord a => STree a -> a -> STree a
insert Nil v = Node Nil v Nil
insert (Node tl x tr) v
    | x == v = Node tl x tr
    | v < x = Node (insert tl v) x tr
    | otherwise = Node tl x (insert tr v)

-- Delete the maximum value from a binary search tree
deletemax :: Ord a => STree a -> (a, STree a)
deletemax (Node tl x Nil) = (x, tl)
deletemax (Node tl x tr) = (y, Node tl x tr')
    where
        (y, tr') = deletemax tr

-- Delete a value from a binary search tree
delete :: Ord a => STree a -> a -> STree a
delete Nil _ = Nil
delete (Node tl x tr) v
    | v < x = Node (delete tl v) x tr
    | v > x = Node tl x (delete tr v)
    | otherwise =
        if tl == Nil then tr
        else Node tl' y tr
        where
            (y, tl') = deletemax tl

-- Create a binary search tree from a list of values
makeTree :: Ord a => [a] -> STree a
makeTree = foldl insert Nil

-- In-order traversal of a binary search tree
inorder :: STree a -> [a]
inorder Nil = []
inorder (Node tl x tr) = inorder tl ++ [x] ++ inorder tr

-- Sort a list using a binary search tree
sort :: Ord a => [a] -> [a]
sort = inorder . makeTree