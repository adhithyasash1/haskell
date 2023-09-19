main :: IO ()
main = do
    putStrLn "Enter a word:"
    word <- getLine
    let reversedWord = reverse word
    putStrLn ("Reversed word: " ++ reversedWord)
