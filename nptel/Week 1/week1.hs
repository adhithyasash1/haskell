-- Consider the following Haskell definition.
-- f x y z = y == ((x+1 == 0) /= z)
-- Which of the following is a possible type of f?

myFunc1 :: Integer -> Bool -> Bool -> Bool
myFunc1 x y z = y == ((x+1 == 0) /= z)

-- Consider the following Haskell function f.
-- What is the value of f 21 35?
-- Answer : 7

myFunc2 :: Int -> Int -> Int
myFunc2 a 0 = a
myFunc2 a b
  | a >= b = myFunc2 b (mod a b)
  | a < b = myFunc2 b a

-- Consider the following Haskell definition.
-- For how many triples (x,y,z) does f x y z evaluate to True?
-- Answer : 1

myFunc3 :: Bool -> Bool -> Bool -> Bool
myFunc3 x y z = (not x || y) && (not y || z) && (z || x)

-- Consider the following Haskell function f.
-- What is the value of f 31 25?
-- Answer : 775

myFunc4 :: Int -> Int -> Int
myFunc4 x y
  | x <= 0 	= 0
  | even x 	= myFunc4 (x `div` 2) (y + y)
  | odd x 	= myFunc4 (x `div` 2) (y + y) + y


myFunc5 :: Bool -> Bool -> Bool -> Bool
myFunc5 x y z = x || not (not y /= z)


-- Reverse a number using recursion

f :: Int -> Int
f n = g n 0

g :: Int -> Int -> Int
g n a
  | n == 0 = a
  | otherwise  = g q (10*a + r)
    where
      q  = div n 10
      r  = mod n 10


-- NOR gate using Wildcard

myFunc6 :: Bool -> Bool -> Bool
myFunc6 True _ = False
myFunc6 _ y = not y
