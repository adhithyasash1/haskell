-- AVL Tree or Balanced Search Tree

-- Binary Search Tree's complexity depends upon the Height of the tree
-- In general, a tree might not be balanced
-- Inserting in ascneding or descending order results in highly skewed trees

{-

makeTree [1..5]

 1
  \
   2
    \ 
     3
      \ 
       4
        \
         5

makeTree [5,4..1]

        5
       /
      4
     /
    3
   /
  2
 /
1

-}

-- BST might take O(n) time single insert/delete/search operation, sequence of such operations still take O(n^2) time, then what is advantage of using them if so?

-- we can manage to balance a tree, so that is of small height, called balanced search tree

-- Ideally we want for each node, the left and right subtrees differ in size by at most 1

	-> height is guaranteed to be logN + 1, where N is the size of the tree

	-> when size is 1, height is also 1 (log1 + 1)

	-> when size is greater than 1, subtrees are of size at most n/2

	-> height = 1 + (log(N/2) + 1) = 1 + (logN -1 + 1) = logN + 1

-- Not easy to maintain size balance, so we will maintain height balance instead which is easier

-- At any node
	-> the left subtree and right subtree differs in height by at most 1

	-> tree rotations to maintain

	-> also called AVL Trees

	-> Height is still O(logN)

{-

height balanced and size balanced

    4
   / \
  2   5
 / \   \ 
1   3   6

height balanced but not size balanced

    4
   / \
  2   5
 / \    
1   3   

-}

-- if a tree has height h, then one of the subtrees is of height h-1
and the other has height at least h-2

-- for a height balanced tree of size N (N nodes), the height is at most 2logN

-- Tree Rotations :

rotateright (Node (Node t1 y t2) x t3) = Node t1 y (Node t2 x t3)

-- we rotate to right by making the root node as right child, and the root node's original left child as root and original left child's right node as new right node's left child.

rotateleft (Node t1 y (Node t2 x t3)) = Node (Node t1 y t2) x t3

-- we rotate to left by making right child of the root node as the new root and original root as its left child and old right child's left child as new left child's right child

-- Logic :

-- Assume tree is currently balanced
-- each insert/delete creates an imbalance
-- fix imbalance using a rebalance function
-- we need to compute height of a tree to check for imbalance

height Nil = 0
height (Node tl x tr) = 1 + max (height tl) (height tr)

-- this computation of height takes O(n) time, so we avoid this implementation in AVL Trees by storing height value.

-- AVL Trees :

{-
each operation takes O(logN) time

sequence of N operations take O(NlogN) time

non-trivial data structure
-}

data AVLTree a = Nil | Node (AVLTree a) a Int (AVLTree a)

height :: AVLTree a -> Int
height Nil = 0
height (Node tl x h tr) = h

-- we also need to measure of how skewed a tree is (ie) slope to measure imbalance, if slope > 2 or slope < -2, then we have imbalance

slope :: AVLTree a -> Int
slope Nil = 0
slope (Node tl x h tr) = height tl - height tr

-- Since we store the height at each node, we need to adjust it after each operation

-- Constant time operation

rotateright :: AVLTree a -> AVLTree a
rotateright (Node (Node tll y hl tlr) x h tr) = Node tll y nh (Node tlr x nhr tr)
	where
	nhr = 1 + max (height tlr) (height tr)
	nh = 1 + max (height tll) nhr

-- tll = left subtree of left subtree
-- tlr = right subtree of left subtree
-- hl = height of left subtree
-- x = root
-- h = height of the tree
-- tr = right subtree of the original tree
-- nh = new height of the tree
-- nhr = new height of the right subtree

rotateleft :: AVLTree a -> AVLTree a
rotateleft (Node tl y h (Node trl x hr trr)) = Node (Node tl y nhl trl) x nh trr
	where
	nhl = 1 + max (height tl) (height trl)
	nh = 1 + max nhl (height tll) nhr

-- tll = left subtree of left subtree
-- tlr = right subtree of left subtree
-- hl = height of left subtree
-- x = root
-- h = height of the tree
-- tr = right subtree of the original tree
-- nh = new height of the tree
-- nhr = new height of the right subtree
-- nhl = new height of left subtree
-- trr = right subtree of right subtree

-- Rebalancing trees

-- in height balanced trees, slope is -1, 0, 1
-- After insert/delete operation, slope can be -2 to 2
-- we rebalance each node on the path visited by operation
-- Constant time operation

rebalance :: AVLTree a -> AVLTree a
rebalance (Node tl x h tr)
	| abs (st) < 2 = Node tl x h tr
	| st == 2 && stl /= -1 = rotateright (Node tl x h tr)
	| st == 2 && stl == -1 = rotateright (Node (rotateright tl) x h tr)
	| st == -2 && str /= 1 = rotateleft (Node tl x h tr)
	| st == -2 && str == 1 = rotateleft (Node tl x h (rotateright tr))
	where
		st = slope (Node tl x h tr)
		stl = slope tl
		str = slope tr

(exercise for the learner to fill out symmetric cases for the rebalance function)

search :: Ord a => AVLTree a -> a -> Bool
search Nil v = False
search (Node tl x h tr) v
	| x == v = True
	| v < x = search tl
	| otherwise = search tr v

-- Time taken = 2logN

insert :: Ord a => AVLTree a -> a -> AVLTree a
insert Nil v = Node Nil v 1 Nil
insert (Node tl x h tr) v
	| x == v = Node tl x h tr
	| v < x = rebalance (Node ntl x nhl tr)
	| otherwise = rebalance (Node tl x nhr ntr)
	where
		ntl = insert tl v
		ntr = insert tr v
		nhl = 1 + max (height ntl) (height tr)
		nhr = 1 + max (height tl) (height ntr)

delete :: Ord a => AVLTree a -> a -> AVLTree a
delete Nil v = Nil
delete (Node tl x h tr) v
	| v < x = rebalance (Node ntl x nhl tr) -- if v is smaller than x, we delete our v from left subtree and get ntl and rebalance
	| v > x = rebalance (Node tl x nhr ntr) -- if v is greater than x, we delete our v from right subtree and get nhr and rebalance
	| otherwise = if (tl == Nil) then tr else rebalance (Node ty y hyr tr) -- rebalance if left subtree is not Nil
	where
		(y, ty) = deletemax tl
		ntl = insert tl v
		ntr = insert tr v
		nhl = 1 + max (height ntl) (height tr)
		nhr = 1 + max (height tl) (height ntr)
		hyr = 1 + max (height ty) (height tr)

deletemax :: AVLTree a -> (a, AVLTree a)
deletemax (Node tl x h Nil) = (x, tl) -- rightmost node
deletemax (Node tl x h tr) = (y, rebalance (Node tl x nh ty)) -- always descend right
	where
		(y, ty) = deletemax tr
		nh = 1 + max (height tl) (height ty)








































































































































































