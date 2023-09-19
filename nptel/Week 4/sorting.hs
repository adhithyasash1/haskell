insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where
    insert :: Ord a => a -> [a] -> [a]
    insert x [] = [x]
    insert x (y:ys)
      | x <= y    = x : y : ys
      | otherwise = y : insert x ys

-- The `insertionSort` function sorts a list of elements using the insertion sort algorithm.
-- The base case is an empty list, which is already sorted.
-- For a non-empty list, it recursively sorts the tail of the list and then inserts the head element at the correct position in the sorted tail.
-- The `insert` function takes an element `x` and a sorted list `ys` and inserts `x` into `ys` at the appropriate position while maintaining the sorted order.


mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs

    merge :: Ord a => [a] -> [a] -> [a]
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

-- The mergeSort function recursively divides the input list into smaller sublists, sorts them, and then merges them together using the merge function.
-- The base cases are empty lists and lists with a single element, both of which are already sorted.
-- The merge function takes two sorted lists and merges them into a single sorted list.


quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]

-- The quickSort function sorts a list of elements using the quicksort algorithm.
-- The base case is an empty list, which is already sorted.
-- For a non-empty list, it selects a pivot element (x in this case), partitions the list into elements smaller and larger than the pivot, and then recursively sorts the smaller and larger partitions.
-- The pivot element is placed between the sorted smaller and larger partitions.

