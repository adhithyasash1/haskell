-- Define a recursive data type 'BTree' for binary trees
data BTree a = Empty | Node (BTree a) a (BTree a)

-- 'Empty' represents an empty tree, 'Node' represents a tree node with a value and references to left and right subtrees

-- Function to calculate the number of nodes in a binary tree
size :: BTree a -> Int
size Empty = 0
size (Node tl x tr) = size tl + 1 + size tr

-- Function to calculate the height of a binary tree
height :: BTree a -> Int
height Empty = 0
height (Node tl x tr) = 1 + max (height tl) (height tr)

-- Function to reflect a binary tree
reflect :: BTree a -> BTree a
reflect Empty = Empty
reflect (Node tl x tr) = Node (reflect tr) x (reflect tl)

-- Function to list all nodes in a binary tree level by level
levels :: BTree a -> [a]
levels t = concat (myLevels t)

-- Helper function for 'levels'
myLevels :: BTree a -> [[a]]
myLevels Empty = []
myLevels (Node t1 x t2) = [x] : join (myLevels t1) (myLevels t2)

-- Helper function to join lists of lists
join :: [[a]] -> [[a]] -> [[a]]
join [] yss = yss
join xss [] = xss
join (xs : xss) (ys : yss) = (xs ++ ys) : join xss yss

-- Custom 'Show' instance for better tree visualization
instance (Show a) => Show (BTree a) where
    show t = drawTree t ""

-- Helper function to draw a tree with proper indentation
drawTree :: (Show a) => BTree a -> String -> String
drawTree Empty spaces = spaces ++ "*\n"
drawTree (Node Nil x Nil) spaces = spaces ++ show x ++ "\n"
drawTree (Node tl x tr) spaces =
    spaces ++ show x ++ "\n" ++
    drawTree tl (' ' : ' ' : spaces) ++
    drawTree tr (' ' : ' ' : spaces)
