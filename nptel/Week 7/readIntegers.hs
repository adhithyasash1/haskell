printOften :: Int -> String -> IO ()
printOften 0 str = return ()
printOften n str = do {
    putStrLn str;
    printOften (n-1) str;
}


main :: IO ()
main = do {
    ls <- readList [];
    putStrLn (show (reverse ls));
}

readList :: [Int] -> IO [Int]
readList l = do {
    inp <- readLn :: IO Int;
    if (inp == -1) then
        return l;
    else readList (inp:l);
}
