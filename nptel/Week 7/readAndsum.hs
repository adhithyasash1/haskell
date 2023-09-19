readListAndSum :: IO ()
readListAndSum = do
    putStrLn "Enter a list of integers (terminate with a negative number):"
    sum <- readListAndSumRec 0
    putStrLn ("Sum of the integers: " ++ show sum)

readListAndSumRec :: Int -> IO Int
readListAndSumRec acc = do
    inp <- readLn :: IO Int
    if inp < 0
        then return acc
        else readListAndSumRec (acc + inp)

main :: IO ()
main = readListAndSum
