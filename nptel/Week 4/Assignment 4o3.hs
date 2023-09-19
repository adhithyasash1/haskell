{- Define a function `countOccurrences :: [Int] -> Int -> Int` 
that takes a list of integers and an integer `n` as input, and 
returns the number of occurrences of `n` in the list. -}

countOccurrences :: [Int] -> Int -> Int
countOccurrences lst n = length (filter (== n) lst)

{- Define a function `maxConsecutiveSum :: [Int] -> Int` that 
takes a list of integers and returns the maximum sum of any 
consecutive subsequence in the list. -}

maxConsecutiveSum :: [Int] -> Int
maxConsecutiveSum lst = maximum [sum sublist | sublist <- subsequences lst]

{- Define a function `listIntersection :: [Int] -> [Int] -> [Int]` that 
takes two lists of integers and returns a list containing elements that 
are common to both input lists. -}

listIntersection :: [Int] -> [Int] -> [Int]
listIntersection xs ys = [x | x <- xs, x `elem` ys]

{- Define a function `isPrime :: Int -> Bool` that takes an 
integer `n` as input and returns `True` if `n` is prime, and `False` otherwise -}

isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = all (\x -> n `mod` x /= 0) [2..isqrt n]
  where
    isqrt = floor . sqrt . fromIntegral

{- Define a function `reverseWords :: String -> String` that 
takes a string as input and returns a new string with the order of words reversed. -}

reverseWords :: String -> String
reverseWords = unwords . reverse . words
