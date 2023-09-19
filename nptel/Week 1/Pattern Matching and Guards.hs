-- Pattern Matching

isItTwo :: Integer -> Bool
isItTwo _ = False
isItTwo 2 = True


returnLast :: a -> (b -> (c -> (d -> d)))
returnLast _ _ _ d = d


-- Guards

myAbsolute :: Integer -> Integer
myAbsolute x
| x < 0 = (-x)
| otherwise = x


isRightTriangle :: (Num a, Eq a) => a -> a -> a -> String
isRightTriangle a b c
| a^2 + b^2 == c^2 = "RIGHT ON"
| otherwise = "not right"


calcDogYears :: (Num a, Ord a) => a -> a
calcDogYears x
| x <= 0 = 0
| x <= 1 = x * 15
| x <= 2 = x * 12
| x <= 4 = x * 8
| otherwise = x * 6


avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
| y >= 0.9 = 'A'
| y >= 0.8 = 'B'
| y >= 0.7 = 'C'
| y >= 0.59 = 'D'
| y < 0.59 = 'F'
where y = x / 100