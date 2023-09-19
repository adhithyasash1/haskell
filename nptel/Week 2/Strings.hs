-- Strings

-- a) checks for occurrence of a character in a string

occurs :: Char -> String -> Bool
occurs c "" = False
occurs c (x:xs)
	| c == x = True
	| otherwise = occurs c xs


position :: Char -> String -> Int
position c "" = 0
position c (x:xs)
 | c == x = 0
 | otherwise = 1 + (position c xs)


-- b) checks for number of words in a string separated by ' ', tab, enter, newline, etc

whitespace :: Char -> Bool
whitespace ' ' = True
whitespace '\t' = True
whitespace '\n' = True
whitespace _ = False

wordcaux :: String -> Int
wordcaux [c] = 0
wordcaux(c:d:ds)
 | (whitespace c) && not (whitespace d) = 1 + wordcaux(d:ds)
 | otherwise = wordcaux(d:ds)

wordc :: String -> Int
wordc s = wordcaux(' ':s)
