module DFA where

import Data.Map.Strict as Map (lookup, fromList)


data DFA = DFA {
    terms :: [Int], 
    root :: Int, 
    delta :: (Int -> Char -> Int)
}


isTerminal (DFA terms _ _) v = elem v terms

processChain (DFA _ root delta) = scanl delta root

buildDelta :: [(Int, Char, Int)] -> Int -> Char -> Int
buildDelta edges = delta where
    delta v c = 
        case Map.lookup (v, c) (Map.fromList $ map toKV edges) of
            (Just v) -> v
            Nothing -> 0
    toKV (x, y, z) = ((x, y), z)

