module DFA where

import Data.Map.Strict as Map (lookup, fromList)


data DFA = DFA {
    terms :: [Int], 
    root :: Int, 
    delta :: (Int -> Char -> Int),
    edges :: [(Int, Char, Int)]
}


isTerminal dfa v = elem v $ terms dfa

processChain dfa = scanl (delta dfa) (root dfa)

buildDelta :: [(Int, Char, Int)] -> Int -> Char -> Int
buildDelta edges = delta where
    delta v c = 
        case Map.lookup (v, c) (Map.fromList $ map toKV edges) of
            (Just v) -> v
            Nothing -> 0
    toKV (x, y, z) = ((x, y), z)

