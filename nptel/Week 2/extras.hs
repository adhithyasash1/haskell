-- snd :: (a, b) -> b
-- snd (1, "hello")  -- Returns "hello"
-- snd (3.14, True)  -- Returns True
-- snd (10, [1, 2, 3])  -- Returns [1, 2, 3]


-- Q: Define a function `sumSquares` that takes a list of integers and returns the sum of squares of all the elements in the list.

-- A:
sumSquares :: [Int] -> Int
sumSquares [] = 0
sumSquares (x:xs) = x^2 + sumSquares xs

-- Q: Write a function `factorial` that takes an integer `n` and returns the factorial of `n`.

-- A:
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Q: Define a function `fibonacci` that takes an integer `n` and returns the nth Fibonacci number.

-- A:
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Q: Write a function `isPalindrome` that takes a string and returns True if it is a palindrome, and False otherwise.

-- A:
isPalindrome :: String -> Bool
isPalindrome str = str == reverse str

-- Q: Define a function `removeDuplicates` that takes a list of integers and removes duplicate elements, keeping only the first occurrence of each element.

-- A:
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

-- Q: Write a function `maximumElement` that takes a list of integers and returns the largest element in the list.

-- A:
maximumElement :: [Int] -> Int
maximumElement [] = error "List is empty."
maximumElement [x] = x
maximumElement (x:xs) = max x (maximumElement xs)
