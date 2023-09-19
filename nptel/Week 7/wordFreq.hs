import Data.Array
import qualified Data.Map as Map

-- Define a function to tokenize a string into words
tokenize :: String -> [String]
tokenize = words

-- Define a list of sentences
sentences :: [String]
sentences =
    [ "This is a sample sentence."
    , "Another sentence here."
    , "And yet another sample sentence."
    ]

-- Function to count word frequencies using accumArray
countWordFrequencies :: [String] -> Map.Map String Int
countWordFrequencies wordsList =
    accumArray (+) 0 ("a", "z") keyValuePairs
  where
    keyValuePairs = [(word, 1) | word <- wordsList]

main :: IO ()
main = do
    let wordList = concatMap tokenize sentences
        wordFreqMap = countWordFrequencies wordList
    putStrLn "Word Frequencies:"
    mapM_ print (Map.toList wordFreqMap)
