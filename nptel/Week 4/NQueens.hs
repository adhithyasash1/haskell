-- Implementation 1

solveNQueens :: Int -> [[Int]]
solveNQueens n = solve n
  where
    solve 0 = [[]]
    solve k = [q : qs | qs <- solve (k - 1), q <- [1..n], isSafe q qs]
    
    isSafe :: Int -> [Int] -> Bool
    isSafe q qs = not $ any (\(col, row) -> q == col || abs (q - col) == length qs) (zip qs [1..])

main :: IO ()
main = do
    let n = 8
    let solutions = solveNQueens n
    putStrLn $ "Solutions for " ++ show n ++ "-Queens:"
    mapM_ print solutions


-- Implementation 2

type Arrangement = [Int]

extend :: Arrangement -> [Arrangement]
extend arr = [arr ++ [col] | col <- [1..n], isSafe col]
  where
    n = length arr
    isSafe col = all (\(c,r) -> col /= c && abs (col - c) /= n - r) $ zip arr [0..]

queens :: Int -> Arrangement
queens n = (iterate (concatMap extend) [[]]) !! (n - 1)

main :: IO ()
main = do
    let n = 8
    let solutions = queens n
    putStrLn $ "Solutions for " ++ show n ++ "-Queens:"
    mapM_ print solutions


-- Data Structure:
  -- Code 1 uses a list of lists to represent arrangements ([[Int]]) and solves the problem using a recursive approach within the solveNQueens function.
  -- Code 2 uses a type alias Arrangement to represent the arrangement of queens in a single row ([Int]) and solves the problem using an iterative approach using the extend and queens functions.

-- Approach:
  -- Code 1 recursively generates solutions by iterating over rows and checking for safe positions in each column within the solveNQueens function.
  -- Code 2 uses an iterative approach, where extend function generates valid extensions of arrangements, and the queens function iteratively generates solutions of increasing length using an infinite list approach.

-- Code Structure:
  -- Code 1 uses nested list comprehensions to generate solutions within the solveNQueens function.
  -- Code 2 separates the extension logic into the extend function and uses iterate to generate solutions within the queens function.
