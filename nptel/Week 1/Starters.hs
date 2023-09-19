-- Palindrome check: This program checks if a given input string is a palindrome (reads the same forwards and backwards).

isPalindrome :: String -> Bool
isPalindrome str = str == reverse str


-- Fibonacci sequence: This program generates the first n numbers of the Fibonacci sequence.

fibonacci :: Int -> [Int]
fibonacci n = take n fibList
  where
    fibList = 0 : 1 : zipWith (+) fibList (tail fibList)


-- Prime number check: This program checks if a given number is a prime number.

isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = null [x | x <- [2..isqrt n], n `mod` x == 0]
  where
    isqrt = floor . sqrt . fromIntegral


-- Reverse a list: This program reverses a list using recursion.

reverseList :: [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]