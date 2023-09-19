-- this whole function can be shortened using map
-- sumLength l = sum(map length l)

sumLength :: [[a]] -> Int
sumLength [] = 0
sumLength (x:xs) = length x + (sumLength xs)


-- we can define map function on our own

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = (f x):(myMap f xs)


-- Selecting element in a list
-- Select all even numbers from a list

even_only :: [Int] -> [Int]
even_only [] = []
even_only (x:xs)
 | is_even x = x:(even_only xs)
 | otherwise = even_only xs
 where
 	is_even :: Int -> Bool
 	is_even x = (mod x 2) == 0

-- This whole thing can be simplified using filter 
-- like the above code can be writte as
-- even_only l = filter is_even l

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p (x:xs)
 | (p x) = x:(myFilter p xs)
 | otherwise = myFilter p xs


-- Combining map and filter is a common practise
-- many often what we want is to extract certain elements of a list which satisfy some property and transform them into some other elements.

-- Extract all vowels and capitalize them

cap_vow :: [Char] -> [Char]
cap_vow l = map touppercase (filter is_vowel l)

is_vowel :: Char -> Bool
is_vowel c = (c == 'a') || (c == 'e') || (c == 'i') || (c == 'o') || (c == 'u')

-- Squares of even numbers in a list

sqr_even :: [Int] -> [Int]
sqr_even l = map sqr (filter is_even l)
