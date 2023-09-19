{-
1. Define a function dropOdds :: Int -> Int with the following behaviour.
For any positive number m, dropOdds m is got by dropping all the odd digits 
in m. (If all the digits in the number are odd, the answer should be 0.)

Test cases:
dropOdds 0 			= 0
dropOdds 8 			= 8
dropOdds 1357 	= 0

2. Define a function moreZeros :: Int -> Bool such that moreZeros n returns 
True exactly when the binary representation of n has strictly more 0s than 1s.

Test cases:
moreZeros 0     = True
moreZeros 1			= False
moreZeros 2     = False
moreZeros 4			= True

3. Define a function binToTer :: Int -> Int which takes as input the binary 
representation of a number n and outputs the ternary representation of n. 
(You can assume that the input consists only of the digits 0 and 1, and the 
output should only consist the digits 0, 1 and 2.)

Test cases:
binToTer 0 			= 0
binToTer 1      = 1
binToTer 11     = 10
binToTer 100    = 11

4. Define a function palindrome :: Int -> Bool which outputs True exactly when 
the number is a palindrome (digits read from left to right is the same as 
digits read from right to left).

Test cases:
palindrome 0		= True
palindrome 121	= True
-}

-- 1. dropOdds function
dropOdds :: Int -> Int
dropOdds 0 = 0
dropOdds n
  | n `mod` 2 == 1 = dropOdds (n `div` 10)
  | otherwise = (n `mod` 10) + 10 * dropOdds (n `div` 10)

-- 2. moreZeros function
countZerosOnes :: Int -> (Int, Int)
countZerosOnes 0 = (0, 0)
countZerosOnes n
  | n `mod` 2 == 0 = (1 + zeros, ones)
  | otherwise = (zeros, 1 + ones)
  where
    (zeros, ones) = countZerosOnes (n `div` 2)

moreZeros :: Int -> Bool
moreZeros 0 = True
moreZeros n = zeros > ones
  where
    (zeros, ones) = countZerosOnes n

-- 3. binToTer function
binToTer :: Int -> Int
binToTer 0 = 0
binToTer n = remainder + 3 * binToTer (n `div` 10)
  where
    remainder = n `mod` 10

-- 4. palindrome function
reverseDigits :: Int -> Int
reverseDigits n = go n 0
  where
    go 0 acc = acc
    go num acc = go (num `div` 10) (acc * 10 + num `mod` 10)

palindrome :: Int -> Bool
palindrome n = n == reverseDigits n
