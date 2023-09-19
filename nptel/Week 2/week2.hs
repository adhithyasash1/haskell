mydrop :: Int -> [Int] -> [Int]
mydrop n l
 | n <= 0 || null l = l
 | otherwise = mydrop (n-1) (tail l)


-- Type checking

l1 :: String
l1 = filter (`elem` ['A'..'Z']) ['a'..'z']

l2 :: [Int]
l2 = [x | x <- [0, 2..10], odd x]

main :: IO ()
-- main = print (l1 == l2)
main = print(l1, l2)


-- myFunc

myFunct :: ([Double], Int)
myFunct = (l, length l)
    where l = [32, 28.3..1]

-- custom definition of take function

myTake n l
 	| n <= 0 || null l	= [ ]
	| otherwise = head l:myTake (n-1) (tail l)

-- GA Q4

g :: Int -> (Int, Int)
g 0 = (0, 0)
g n = let (x, y) = g (n - 1)
       in (x + 1, y - x)

f :: Int -> Int
f = snd . g

-- GA Q3

myfunc :: (Int -> Int) -> [Int] -> Int
myfunc k [] = k 100
myfunc k (x:xs) = myfunc ((x*) . k) xs

myfunc1 :: [Int] -> Int
myfunc1 = myfunc (\x -> x)

myMain :: IO ()
myMain = print (myfunc1 [1..5])
