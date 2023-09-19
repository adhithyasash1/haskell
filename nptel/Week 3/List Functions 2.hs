-- Computes the dot product of two lists using foldr and zipWith
dotProduct :: [Int] -> [Int] -> Int
dotProduct xs ys = foldr (+) 0 $ zipWith (*) xs ys


-- Reverses a list using foldl
reverseList :: [a] -> [a]
reverseList xs = foldl (\acc x -> x : acc) [] xs


-- Flattens a list of lists using concatMap
flattenLists :: [[a]] -> [a]
flattenLists xs = concatMap id xs


-- Finds the longest consecutive increasing sequence in a list using foldr1 and takeWhile
longestIncreasingSequence :: [Int] -> [Int]
longestIncreasingSequence xs = foldr1 (\x acc -> if x < head acc then [x] else x : acc) $ takeWhile (\(x, y) -> x < y) $ zip xs (tail xs)


-- Simulates the height of a bouncing ball until it reaches a certain threshold using takeWhile and concatMap
bounceSimulation :: Float -> Float -> [Float]
bounceSimulation initialHeight elasticity = takeWhile (\h -> h > 0.1) $ iterate (\h -> h * elasticity) initialHeight


-- Generates the Fibonacci sequence using zipWith and iterate
fibonacci :: [Int]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)
