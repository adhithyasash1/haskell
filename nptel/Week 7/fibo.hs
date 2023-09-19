import Data.Array

fib :: Int -> Integer
fib n = fibA ! n
  where
    fibA :: Array Int Integer
    fibA = listArray (0, n) [f i | i <- [0..n]]
    
    f 0 = 0
    f 1 = 1
    f i = fibA ! (i - 1) + fibA ! (i - 2)

main :: IO ()
main = do
    putStrLn "Enter the Fibonacci index:"
    index <- readLn :: IO Int
    putStrLn ("Fibonacci number at index " ++ show index ++ " is " ++ show (fib index))
