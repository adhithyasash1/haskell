import Data.Array

lcss :: String -> String -> Int
lcss str1 str2 = lcssA ! (0, 0)
  where
    m = length str1
    n = length str2
    ar1 = listArray (0, m - 1) str1
    ar2 = listArray (0, n - 1) str2
    lcssA = array ((0, 0), (m, n)) [((i, j), f i j) | i <- [0..m], j <- [0..n]]
    
    f i j
      | i >= m || j >= n = 0
      | ar1 ! i == ar2 ! j = 1 + lcssA ! (i + 1, j + 1)
      | otherwise = max (lcssA ! (i, j + 1)) (lcssA ! (i + 1, j))
    
main :: IO ()
main = do
  let lcssResult = lcss "AGGTAB" "GXTXAYB"
  putStrLn $ "Longest Common Subsequence: " ++ show lcssResult
