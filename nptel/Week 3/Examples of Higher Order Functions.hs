-- Generates all partitions of a list
partitions :: [a] -> [[[a]]]
partitions [x] = [[x]]
partitions (x:xs) = [(x:head l):(tail l) | l <- partitions xs] ++ [[x]:l | l <- partitions xs]


-- Generates initial segments of a list
initsegs :: [a] -> [[a]]
initsegs [] = [[]]
initsegs (x:xs) = [] : map (x:) (initsegs xs)


-- Generates all possible interleavings of an element into a list
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)


-- Generates all permutations of a list
perm :: [a] -> [[a]]
perm [x] = [[x]]
perm (x:xs) = concatMap (interleave x) (perm xs)


-- Generates all possible powersets of a list
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)


-- Generates all possible sums of subsets of a list
subsetSums :: [Int] -> [Int]
subsetSums [] = [0]
subsetSums (x:xs) = let rest = subsetSums xs in rest ++ map (+x) rest


-- Generates all binary strings of length n
binaryStrings :: Int -> [String]
binaryStrings 0 = [""]
binaryStrings n = concatMap (\x -> map (x:) (binaryStrings (n-1))) ["0", "1"]


-- Generates all unique substrings of a given string
uniqueSubstrings :: String -> [String]
uniqueSubstrings [] = []
uniqueSubstrings (x:xs) = nub (map (x:) (inits xs)) ++ uniqueSubstrings xs


-- Generates all anagrams of a given string
anagrams :: String -> [String]
anagrams [] = [[]]
anagrams xs = concatMap (\x -> map (x:) (anagrams (delete x xs))) xs

