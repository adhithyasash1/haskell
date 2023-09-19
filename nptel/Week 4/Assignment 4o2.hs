{- Define a function `f5 :: [Int] -> Int -> [Int]` which takes a 
list of integers and an integer `n` as input. The function should 
replace each element in the list that is divisible by `n` 
with the result of dividing it by `n`. 

mycode
f5 :: [Int] -> Int -> [Int]
f5 [] n = []
f5 (x:xs) n
 | mod x n == 0 = (x `div` n):f5 xs n
 | otherwise = 0:f5 xs n
-}

f5 :: [Int] -> Int -> [Int]
f5 lst n = [if x `mod` n == 0 then x `div` n else x | x <- lst]

{- Define a function `f6 :: [Int] -> Int -> [Int]` which takes 
a list of integers and an integer `k` as input. The function 
should remove all elements from the list that occur exactly `k` times, 
keeping only the elements that occur a different number of times. -}

import Data.List (group, sort)

f6 :: [Int] -> Int -> [Int]
f6 lst k = [x | x <- lst, length (filter (== x) lst) /= k]

{- Define a function `f7 :: [Int] -> [Int]` that takes a 
list of integers and returns a new list where each element 
is the sum of all the elements in the input list that 
are to the right of the current element. -}

f7 :: [Int] -> [Int]
f7 lst = [sum (drop (i + 1) lst) | (i, _) <- zip [0..] lst]

{- Define a function `f8 :: [Int] -> Int` that takes 
a list of integers and returns the sum of the maximum 
and minimum elements in the list. -}

f8 :: [Int] -> Int
f8 lst = maximum lst + minimum lst

{- Define a function `f9 :: [Int] -> Int -> [Int]` that 
takes a list of integers and an integer `k` as input. 
The function should remove every `k`th element from the 
list, keeping the remaining elements in the same order. -}

f9 :: [Int] -> Int -> [Int]
f9 lst k = [x | (i, x) <- zip [1..] lst, i `mod` k /= 0]