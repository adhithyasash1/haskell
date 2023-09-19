-- Insertion Sort using Type class

insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert x ys

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)



-- Merge Sort using Type class

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort firstHalf) (mergeSort secondHalf)
  where
    (firstHalf, secondHalf) = splitAt (length xs `div` 2) xs



-- Quick Sort using Type class

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort [y | y <- xs, y <= x] ++ [x] ++ quickSort [y | y <- xs, y > x]


-- Main 

main :: IO ()
main = do
    let intList = [5, 2, 10, 1, 8, 3]
    putStrLn "Insertion Sort:"
    print $ insertionSort intList
    
    let charList = "haskell"
    putStrLn "Merge Sort:"
    print $ mergeSort charList
    
    let floatList = [3.14, 1.41, 2.71, 0.618]
    putStrLn "Quick Sort:"
    print $ quickSort floatList
