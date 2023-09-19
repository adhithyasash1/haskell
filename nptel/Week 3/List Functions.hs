-- Adds corresponding elements of two lists using zipWith
addLists :: [Int] -> [Int] -> [Int]
addLists xs ys = zipWith (+) xs ys


-- Takes elements from a list while they are less than 5
takeWhileLessThan5 :: [Int] -> [Int]
takeWhileLessThan5 xs = takeWhile (< 5) xs


-- Sums up the elements of a list using foldr
sumList :: [Int] -> Int
sumList xs = foldr (+) 0 xs


-- Calculates the factorial of a number using foldl
factorial :: Int -> Int
factorial n = foldl (*) 1 [1..n]


-- Finds the maximum element in a list using foldr1
maxElement :: [Int] -> Int
maxElement xs = foldr1 max xs


-- Duplicates each element in a list using concatMap
duplicateElements :: [Int] -> [Int]
duplicateElements xs = concatMap (\x -> [x, x]) xs


-- Given two lists, multiplies corresponding elements and sums them until the sum exceeds a threshold
sumMultiplesUntilThreshold :: Int -> [Int] -> [Int] -> Int
sumMultiplesUntilThreshold threshold xs ys =
    let multiplied = zipWith (*) xs ys
        summed = foldr (+) 0 multiplied
    in sum $ takeWhile (<= threshold) multiplied


-- Takes a list of words and concatenates them in reverse order using foldl and concatMap
concatenateWordsReverse :: [String] -> String
concatenateWordsReverse words = foldl (\acc word -> word ++ acc) "" words


-- Using concatMap to generate a list of prime factors for each number in a list
primeFactorsList :: [Int] -> [[Int]]
primeFactorsList numbers = concatMap primeFactors numbers
    where
        primeFactors n = factorize n primes
        factorize n (p:ps)
            | p * p > n      = [n]
            | n `mod` p == 0 = p : factorize (n `div` p) (p:ps)
            | otherwise      = factorize n ps
        primes = sieve [2..]
            where
                sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

