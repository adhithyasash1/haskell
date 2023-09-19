accumArray
  :: Ix i
  => (e -> a -> e)  -- Accumulating function
  -> e              -- Initial entry (at each index)
  -> (i, i)         -- Bounds of the array
  -> [(i, a)]       -- Association list
  -> Array i e      -- Resulting array

accumArray (+) 0 ('a', 'd') [('a', 2), ('b', 3), ('a', 2), ('c', 4)]
-- Output: array ('a','d') [('a',4),('b',3),('c',4),('d',0)]

accumArray (+) 0 (1, 3) [(1, 1), (2, 1), (2, 1), (1, 1), (3, 1), (2, 1)]
-- Output: array (1,3) [(1,2),(2,3),(3,1)]

counts :: [Int] -> [(Int, Int)]
counts xs = assocs (accumArray (+) 0 (l, u) (zip xs ones))
  where
    ones = iterate id 1
    l = minimum xs
    u = maximum xs

arraysort :: [Int] -> [Int]
arraysort xs = concat [replicate n i | (i, n) <- ys]
  where
    ys = counts xs
