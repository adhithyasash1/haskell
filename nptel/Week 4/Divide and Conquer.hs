binarySearch :: Ord a => [a] -> a -> Bool
binarySearch [] _ = False
binarySearch [x] target = x == target
binarySearch xs target
    | target < midValue = binarySearch leftHalf target
    | target > midValue = binarySearch rightHalf target
    | otherwise = True
  where
    midIndex = length xs `div` 2
    midValue = xs !! midIndex
    (leftHalf, rightHalf) = splitAt midIndex xs


matrixMultiply :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultiply [] _ = []
matrixMultiply (row:rows) cols = [sum (zipWith (*) row col) | col <- transpose cols] : matrixMultiply rows cols
