-- Basic Functions
identity x = id x
constExample x y = const x y
composeExample f g x = (f . g) x

-- List Functions
listHeadExample xs = head xs
listTailExample xs = tail xs
listInitExample xs = init xs
listLastExample xs = last xs
listLengthExample xs = length xs
listNullExample xs = null xs
listTakeExample n xs = take n xs
listDropExample n xs = drop n xs
listElemExample x xs = elem x xs
listFilterExample f xs = filter f xs
listMapExample f xs = map f xs
listFoldlExample f acc xs = foldl f acc xs
listFoldrExample f acc xs = foldr f acc xs
listZipExample xs ys = zip xs ys

-- Tuple Functions
tupleFstExample (x, _) = fst x
tupleSndExample (_, y) = snd y
tupleZipWithExample f xs ys = zipWith f xs ys

-- Numeric Functions
numericAbsExample x = abs x
numericMaxExample x y = max x y
numericMinExample x y = min x y
numericSumExample xs = sum xs
numericProductExample xs = product xs

-- Higher-order Functions
higherOrderCurryExample f x y = curry f x y
higherOrderUncurryExample f (x, y) = uncurry f (x, y)
higherOrderFlipExample f x y = flip f y x

-- IO Functions (These can be used in a Haskell program, not in this specific snippet)
ioPutStrLnExample = putStrLn "Hello, Haskell!"
ioGetLineExample = do
  putStrLn "Enter your name: "
  name <- getLine
  putStrLn $ "Hello, " ++ name

-- Maybe Functions
maybeExample f x = maybe 0 f x

-- Either Functions
eitherExample f g x = either f g x

main = do
  -- Example usage of some functions
  print $ identity 42
  print $ constExample 10 20
  print $ composeExample (+1) (*2) 5

  let myList = [1, 2, 3, 4, 5]
  print $ listHeadExample myList
  print $ listTailExample myList
  print $ listInitExample myList
  print $ listLastExample myList
  print $ listLengthExample myList
  print $ listNullExample myList
  print $ listTakeExample 3 myList
  print $ listDropExample 2 myList
  print $ listElemExample 3 myList
  print $ listFilterExample even myList
  print $ listMapExample (*2) myList
  print $ listFoldlExample (+) 0 myList
  print $ listFoldrExample (+) 0 myList
  print $ listZipExample myList [10, 20, 30, 40]

  let myTuple = (42, "Haskell")
  print $ tupleFstExample myTuple
  print $ tupleSndExample myTuple
  print $ tupleZipWithExample (+) [1, 2, 3] [10, 20, 30]

  print $ numericAbsExample (-10)
  print $ numericMaxExample 5 8
  print $ numericMinExample 5 8
  print $ numericSumExample myList
  print $ numericProductExample myList

  let addTwoNumbers x y = x + y
  let curriedAdd = higherOrderCurryExample addTwoNumbers
  print $ curriedAdd 10 5

  let uncurriedAdd = higherOrderUncurryExample addTwoNumbers
  print $ uncurriedAdd (10, 5)

  let flippedAdd = higherOrderFlipExample addTwoNumbers
  print $ flippedAdd 10 5

  -- Example usage of IO functions
  ioPutStrLnExample

  -- Uncomment the line below to run the ioGetLineExample function
  -- ioGetLineExample

  let maybeValue = Just 42
  print $ maybeExample (+1) maybeValue

  let eitherValue = Right 42
  print $ eitherExample (*2) (+1) eitherValue
