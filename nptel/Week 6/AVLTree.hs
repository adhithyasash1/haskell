-- Define the AVL tree data structure
data AVLTree a = Nil | Node (AVLTree a) a Int (AVLTree a)

-- 'Nil' represents an empty tree, 'Node' represents a node with left and right subtrees and their heights

-- 'Int' is used to store the height of the tree at each node

{-
We define the AVL tree data structure using a custom AVLTree type. Each node can be either empty (Nil) or a node (Node) containing a value, the left subtree, the right subtree, and the height of the node.
-}

-- Function to calculate the height of an AVL tree
height :: AVLTree a -> Int
height Nil = 0
height (Node _ _ h _) = h

{-
This function calculates the height of an AVL tree. If the tree is empty (Nil), its height is 0. Otherwise, it retrieves the height value stored in the node.
-}

-- Function to calculate the slope of an AVL tree
slope :: AVLTree a -> Int
slope Nil = 0
slope (Node tl _ _ tr) = height tl - height tr

{-
The slope function calculates the difference in height between the left subtree and the right subtree of a node. It returns a positive value if the left subtree is taller, a negative value if the right subtree is taller, and 0 if they are of equal height.
-}

-- Function to perform a right rotation on an AVL tree
rotateright :: AVLTree a -> AVLTree a
rotateright (Node (Node tll y hl tlr) x h tr) = Node tll y nh (Node tlr x nhr tr)
  where
    nhr = 1 + max (height tlr) (height tr)
    nh = 1 + max (height tll) nhr

{-
The rotateright function performs a right rotation on an AVL tree to restore balance. It takes a node with a left-heavy imbalance and rotates it to the right. The heights of the affected nodes are updated to maintain balance.
-}

-- Function to perform a left rotation on an AVL tree
rotateleft :: AVLTree a -> AVLTree a
rotateleft (Node tl y h (Node trl x hr trr)) = Node (Node tl y nhl trl) x nh trr
  where
    nhl = 1 + max (height tl) (height trl)
    nh = 1 + max nhl (height trr)

{-
The rotateleft function performs a left rotation on an AVL tree to restore balance. It takes a node with a right-heavy imbalance and rotates it to the left. The heights of the affected nodes are updated to maintain balance.
-}

-- Function to rebalance an AVL tree
rebalance :: AVLTree a -> AVLTree a
rebalance (Node tl x h tr)
  | abs st < 2 = Node tl x h tr
  | st == 2 && stl /= -1 = rotateright (Node tl x h tr)
  | st == 2 && stl == -1 = rotateright (Node (rotateright tl) x h tr)
  | st == -2 && str /= 1 = rotateleft (Node tl x h tr)
  | st == -2 && str == 1 = rotateleft (Node tl x h (rotateright tr))
  where
    st = slope (Node tl x h tr)  -- Slope of the current node
    stl = slope tl               -- Slope of the left subtree
    str = slope tr               -- Slope of the right subtree

{-
The rebalance function checks if a node in an AVL tree is unbalanced (has a slope greater than 1 or less than -1). If it's unbalanced, it applies the necessary rotations to restore balance and returns the balanced tree. This function ensures that the height difference between left and right subtrees is at most 1.
-}

-- Function to insert a value into an AVL tree
insert :: Ord a => AVLTree a -> a -> AVLTree a
insert Nil v = Node Nil v 1 Nil
insert (Node tl x h tr) v
  | x == v = Node tl x h tr
  | v < x = rebalance (Node (insert tl v) x nh tr)
  | otherwise = rebalance (Node tl x nh (insert tr v))
  where
    ntl = insert tl v  -- Insert into the left subtree
    ntr = insert tr v  -- Insert into the right subtree
    nhl = 1 + max (height ntl) (height tr)  -- Update height of left subtree
    nhr = 1 + max (height tl) (height ntr)  -- Update height of right subtree
    nh = 1 + max nhl nhr                   -- Update height of the current node

{-
The insert function adds a new value to an AVL tree while maintaining balance. It first recursively inserts the value into the appropriate subtree and then rebalances the tree if necessary to ensure height balance.
-}

-- Function to delete a value from an AVL tree
delete :: Ord a => AVLTree a -> a -> AVLTree a
delete Nil _ = Nil
delete (Node tl x h tr) v
  | v < x = rebalance (Node (delete tl v) x nh tr)  -- Delete from the left subtree
  | v > x = rebalance (Node tl x nh (delete tr v))  -- Delete from the right subtree
  | otherwise = if tl == Nil then tr else rebalance (Node ty y hyr tr)
  where
    (y, ty) = deletemax tl        -- Find the maximum value in the left subtree
    ntl = insert tl v             -- Insert the value into the left subtree
    ntr = insert tr v             -- Insert the value into the right subtree
    nhl = 1 + max (height ntl) (height tr)
    nhr = 1 + max (height tl) (height ntr)
    nh = 1 + max nhl nhr
    hyr = 1 + max (height ty) (height tr)

{-
The delete function removes a value from an AVL tree while maintaining balance. It uses a helper function deletemax to find the maximum value in the left subtree (the predecessor of the current node), removes it from the tree, and then rebalances the tree to ensure height balance.
-}

-- Function to delete the maximum value from an AVL tree and return it
deletemax :: AVLTree a -> (a, AVLTree a)
deletemax (Node tl x _ Nil) = (x, tl)  -- Rightmost node
deletemax (Node tl x h tr) = (y, rebalance (Node tl x nh ty))
  where
    (y, ty) = deletemax tr        -- Recursively find the maximum value in the right subtree
    nh = 1 + max (height tl) (height ty)

{-
The deletemax function finds and removes the maximum value from an AVL tree, returning the deleted value and the modified tree. It ensures that the tree remains balanced by rebalancing after the deletion.
-}

{-
AVL trees guarantee efficient search, insert, and delete operations with a time complexity of O(log N) for each operation, making them a powerful data structure for ordered data.
-}