type Path = [Char]

extendPath :: Path -> [Path]
extendPath [] = [[c] | c <- ['A'..'F']]
extendPath p = [p ++ [c] | c <- ['A'..'F'], edge (last p) c]

edge :: Char -> Char -> Bool
edge a b = True -- Placeholder function, you can define your own logic

extendAll :: [Path] -> [Path]
extendAll [] = [[c] | c <- ['A'..'F']]
extendAll l = concat [extendPath p | p <- l]

iterateFunction :: (a -> a) -> a -> [a]
iterateFunction f x = x : iterateFunction f (f x)

allPaths :: [[Path]]
allPaths = iterateFunction extendAll [[]]

n :: Int
n = length ['A'..'F']

firstNPaths :: [[Path]]
firstNPaths = drop 2 (take (n+1) allPaths)

connectedPairs :: [(Char, Char)]
connectedPairs = [(head p, last p) | l <- firstNPaths, p <- l]

connected :: Char -> Char -> Bool
connected x y = elem (x, y) connectedPairs

