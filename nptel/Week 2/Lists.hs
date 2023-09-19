-- Lists

myLength :: [Int] -> Int
myLength [] = 0
myLength l = 1 + myLength(tail l)


myLengthPat :: [Int] -> Int
myLengthPat [] = 0
myLengthPat(x:xs) = 1 + myLengthPat xs


mySum :: [Int] -> Int
mySum [] = 0
mySum(x:xs) = x + mySum(xs)


reversel :: [Int] -> [Int]
reversel [] = []
reversel(x:xs) = reversel(xs) ++ [x]


appendr :: Int -> [Int] -> [Int]
appendr x [] = [x]
appendr x (y:ys) = y:(appendr x ys)


ascending :: [Int] -> Bool
ascending [] = True
ascending [x] = True
ascending (x:y:ys) = (x <= y) && ascending(y:ys)


-- Slightly more involved samples on lists

alternating :: [Int] -> Bool
alternating l = (updown l) || (downup l)

updown :: [Int] -> Bool
updown [] = True
updown [x] = True
updown(x:y:ys) = (x < y) && (downup(y:ys))

downup :: [Int] -> Bool
downup [] = True
downup [x] = True
downup(x:y:ys) = (x > y) && (updown(y:ys))


mytake :: Int -> [Int] -> [Int]
mytake n [] = []
mytake n x:xs
	| n <= 0 = []
	| otherwise = x:(mytake (n-1) xs)