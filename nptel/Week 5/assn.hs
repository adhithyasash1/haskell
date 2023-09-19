subSeq :: String -> String -> Bool
subSeq [] _ = True
subSeq _ [] = False
subSeq (x:xs) (y:ys)
    | x == y = subSeq xs ys
    | otherwise = subSeq (x:xs) ys

subWord :: String -> String -> Bool
subWord "" _ = True
subWord _ "" = False
subWord (x:xs) (y:ys)
    | x == y = subWord xs ys
    | otherwise = subWord (x:xs) ys

isMatrix :: [[a]] -> Bool
isMatrix [] = False
isMatrix [[],[],[]] = False
isMatrix (x:xs)
    | any (\row -> length row /= headLen) xs = False
    | otherwise = True
  where
    headLen = length x

isSquareMatrix :: [[a]] -> Bool
isSquareMatrix [] = False
isSquareMatrix [[]] = False
isSquareMatrix (x:xs)
    | any (\row -> length row /= numRows) xs = False
    | otherwise = True
  where
    numRows = length (x:xs)

addable :: [[a]] -> [[a]] -> Bool
addable m1 m2
    | length m1 /= length m2 = False
    | any (\(row1, row2) -> length row1 /= length row2) (zip m1 m2) = False
    | otherwise = True

addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices [] [] = []
addMatrices (x:xs) (y:ys)
    | addable (x:xs) (y:ys) = doSum x y : addMatrices xs ys
    | otherwise = []

multiplyable :: [[a]] -> [[a]] -> Bool
multiplyable m1 m2
    | isSquareMatrix m1 && isSquareMatrix m2 = True
    | isMatrix m1 && isMatrix m2 && length m2 == length (head m1) = True
    | isMatrix m1 && isMatrix m2 && length (head m1) == length m2 = True
    | otherwise = False

doSum :: [Int] -> [Int] -> [Int]
doSum xs ys = zipWith (+) xs ys

multiplyMatrices :: [[Int]] -> [[Int]] -> [[Int]]
multiplyMatrices [] _ = []
multiplyMatrices _ [] = []
multiplyMatrices m1 m2
    | not (multiplyable m1 m2) = error "Matrices are not compatible for multiplication"
    | otherwise = [ [ sum $ zipWith (*) row col | col <- transpose m2 ] | row <- m1 ]
    where
        transpose :: [[a]] -> [[a]]
        transpose ([]:_) = []
        transpose x = map head x : transpose (map tail x)
