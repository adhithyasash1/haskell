import Data.Array

fib :: Int -> Integer
fib n = fibA ! n

fibA :: Array Int Integer
fibA = listArray (0, n) [f i | i <- [0..n]]
  where
    n = 1000 -- Set 'n' to the desired Fibonacci number you want to calculate.
    
    f 0 = 1
    f 1 = 1
    f i = fibA ! (i - 1) + fibA ! (i - 2)
    
main :: IO ()
main = do
  let fibResult = fib 10 -- Calculate Fibonacci(10)
  putStrLn $ "Fibonacci(10) = " ++ show fibResult
