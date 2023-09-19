{- 1. Define a function f1 :: [Int] -> [Int] which takes a list l of nonnegative 
numbers as input, and replaces each n in l by 3*n if n is a power of 3, 
and by 0 if it is not a power of 3. 

Examples:
  f1 [] = []
  f1 [1] = [3]
  f1 [1, 2, 3] = [3, 0, 9]
  f1 [0, 2, 4, 6] = [0, 0, 0, 0] 

1. The function signature `f1 :: [Int] -> [Int]` indicates that 
`f1` takes a list of integers as input and returns a list of integers.

2. The base case `f1 [] = []` handles the case when the 
input list is empty. In this case, the result is an empty list.

3. The function `f1` is defined using a list comprehension 
that iterates through each element `n` in the input list `lst`.

4. For each element `n`, the comprehension uses the condition 
`if isPowerOfThree n then 3 * n else 0`. If `n` is a power of 3, 
it is replaced with `3 * n`. Otherwise, it is replaced with 0.

5. The `isPowerOfThree` function is defined separately. 
It takes an integer `n` and returns a boolean indicating 
whether `n` is a power of 3.

6. The `isPowerOfThree` function uses a helper function 
`isPowerOfThreeHelper` to check whether `n` is a power of 3. 
This helper function is called with two arguments: 
s`n` and `p`, where `p` is initialized to 1.

7. The `isPowerOfThreeHelper` function compares `n` and `p` in the following way:
   - If `n` is equal to `p`, it means that `n` is a power of 3, 
     and the function returns `True`.
   
   - If `n` is less than `p`, it means that `n` is not 
     a power of 3, and the function returns `False`.
   
   - If none of the above conditions hold, the function recursively calls 
     itself with `n` and `p * 3`, checking the next power of 3.

8. The `isPowerOfThree` function also includes cases to handle 
special values like 0 and negative numbers, which are not powers of 3. -}

f1 :: [Int] -> [Int]
f1 [] = []
f1 lst = [if isPowerOfThree n then 3 * n else 0 | n <- lst]
  where
    isPowerOfThree n = n > 0 && isPowerOfThreeHelper n 1
    isPowerOfThreeHelper n p
      | n == p    = True
      | n < p     = False
      | otherwise = isPowerOfThreeHelper n (p * 3)

isPowerOfThree :: Int -> Bool
isPowerOfThree n
  | n <= 0    = False   -- 0 and negative numbers are not powers of 3
  | n == 1    = True    -- 3^0 = 1
  | otherwise = n `mod` 3 == 0 && isPowerOfThree (n `div` 3)













{- 2. For a list l, define S(l) to be the set of all indices i of l (remember that 
indices start from 0) such that l!!i > l!!(i+1). Define a function 
f2 :: [Int] -> [Int] which takes a nonempty list l of 
integers as input and outputs a S(l) in order.

Examples:
  f2 [] = []
  f2 [1] = []
  f2 [1, 2, 3, 2, 1] = [2, 3]
  f2 [1, 2, 3, 4, 5, 6] = [] 

1. The function signature `f2 :: [Int] -> [Int]` indicates that 
`f2` takes a list of integers as input and returns a list of integers.

2. In the function definition, `xs` represents the input list of integers.

3. The expression `(zip xs (tail xs))` creates a list of pairs, 
where each pair consists of an element from the input list `xs` 
and its adjacent element (the next element in the list).

4. The expression `zip [0..] (zip xs (tail xs))` pairs each 
pair of elements with their corresponding index in the input list. 
This will give a list of pairs where each pair is of 
the form `(index, (element, adjacent_element))`.

5. The list comprehension `[i | (i, (x,y)) <- zip [0..] (zip xs (tail xs)), x > y]` 
iterates through each pair `(i, (x,y))` in the zipped list. 
Here, `i` is the index, `x` is the current element, 
and `y` is the adjacent element.

6. The condition `x > y` checks if the current element `x` 
is greater than its adjacent element `y`.

7. If the condition is true, the index `i` is included in 
the resulting list. This means that the index `i` is added 
to the list if the element at that index in the input list 
is greater than its adjacent element.

8. The resulting list contains the indices where this condition 
holds true, indicating that the corresponding elements in the 
input list are greater than their adjacent elements.-}

f2 :: [Int] -> [Int]
f2 xs = [i | (i, (x,y)) <- zip [0..] (zip xs (tail xs)), x > y]















{- 3. Define a function f3 :: [Int] -> [Int] that removes adjacent duplicates. 
i.e. if the same element occurs n times contiguously, we retain only one copy.

Examples:
  f3 [1, 1, 1, 2, 2, 3, 3, 3, 3] = [1, 2, 3]
  f3 [1, 2, 1, 2, 3, 1, 1, 2, 2] = [1, 2, 1, 2, 3, 1, 2] -}

f3 :: [Int] -> [Int]
f3 [] = []
f3 [x] = [x]
f3 (x:y:xs)
  | x == y    = f3 (y:xs)
  | otherwise = x : f3 (y:xs)
















{- 4. Define a function f4 :: [Int] -> [[Int]] that partitions the list into all 
its upruns. An uprun is a maximal non-decreasing segment of the given list. 

Examples:

f4 [] = []
f4 [5] = [[5]]
f4 [1, 2, 3, 4, 5] = [[1,2,3,4,5]]
f4 [1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1] = [[1,2,3,4,5,6],[5],[4],[3],[2],[1]] 

1. The function signature `f4 :: [Int] -> [[Int]]` indicates that 
`f4` takes a list of integers as input and returns a list of lists of integers.

2. The base case `f4 [] = []` handles the case when the input list 
is empty. In this case, the result is an empty list of lists.

3. The function is defined recursively using a helper function 
`f4Helper` that takes two arguments: the remaining portion of 
the input list and an accumulator list that keeps track of the 
current uprun being formed.

4. The `f4Helper` function has three cases:
   - When the input list is empty, the accumulator list is reversed 
     and wrapped in a singleton list to represent the last uprun.
   
   - When the input list has only one element `x`, the element is 
     added to the accumulator, then the accumulator is reversed and 
     wrapped in a singleton list.
   
   - When the input list has at least two elements `(x:y:xs)`, 
     two possibilities are considered:
      
        - If `x` is less than or equal to `y`, it means the uprun continues. 
          The helper function is recursively called with `y:xs` and `(x:acc)` 
          as arguments to extend the current uprun.
        
        - If `x` is greater than `y`, the current uprun is complete. 
          The accumulated elements are reversed and added as a new uprun, 
          and the helper function is recursively called 
          with `y:xs` and an empty accumulator.

In summary, the code works by traversing through the input list and 
forming upruns using the `f4Helper` function. Whenever an element 
violates the non-decreasing order, the current uprun is completed 
and stored, and a new uprun begins. -}

f4 :: [Int] -> [[Int]]
f4 [] = []
f4 lst = f4Helper lst []
  where
    f4Helper [] acc = [reverse acc]
    f4Helper [x] acc = [reverse (x:acc)]
    f4Helper (x:y:xs) acc
      | x <= y    = f4Helper (y:xs) (x:acc)
      | otherwise = reverse (x:acc) : f4Helper (y:xs) []