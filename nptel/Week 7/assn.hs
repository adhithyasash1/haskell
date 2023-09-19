{-
In this assignment you have to submit a standalone Haskell program that can 
be compiled using the ghc compiler. 

The input to the program will be multiple lines. Each input line is guaranteed 
to be a single word, with no beginning or trailing spaces. On reading a line, 
your program should print "Y"  if the word is a palindrome (after converting 
every letter to lowercase), and "N" if not. 

A palindrome is a string that is the same as its reverse.

Here is a sample run:

Input
-----
abba
Level
Hi
Malayalam 

Output
------
Y
Y
N
Y
-}

-- Function to check if a string is a palindrome
isPalindrome :: String -> Bool
isPalindrome s = cleanString == reverse cleanString
  where
    cleanString = map toLower' s
    toLower' c
      | 'A' <= c && c <= 'Z' = toLowerChar c
      | otherwise = c
    toLowerChar c = toEnum (fromEnum c + 32)

-- Function to process input lines and print "Y" or "N" accordingly
processLines :: [String] -> IO ()
processLines [] = return ()
processLines (line : lines) = do
  let result = if isPalindrome line then "Y" else "N"
  putStrLn result
  processLines lines

main :: IO ()
main = do
  input <- getContents
  let linesOfInput = lines input
  processLines linesOfInput
