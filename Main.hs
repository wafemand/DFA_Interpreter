import System.Environment
import DFA


main :: IO()
main = do
    [dfaFile, inputFile, outputFile] <- getArgs
    rawDFA <- readFile dfaFile
    input <- readFile inputFile
    output <- return (process rawDFA input)
    writeFile outputFile output
    return()


process :: String -> String -> String
process rawDFA input = showTex dfa chains results where
    dfa = parseDFA rawDFA
    chains = words input
    results = map (processChain dfa) chains


-- Parsing input

edgesHeader = "Edges:"
termsHeader = "Terminals:"
parseDFA :: String -> DFA
parseDFA input = DFA terms root delta where
    root = read $ head wds
    delta = buildDelta $ map parseEdge $ group3 $ tail edgesStr
    terms = map (read) $ tail termsStr
    wds = words input
    (edgesStr, termsStr) = break (==termsHeader) $ snd $ break (==edgesHeader) wds
    parseEdge (x,y,z) = (read x, head y, read z)
    group3 :: [t] -> [(t, t, t)]
    group3 [] = []
    group3 (x:y:z:xs) = (x, y, z) : group3 xs
    group3 _ = error "length doesn't divide by 3"


-- LaTeX 

texHeader = "\\documentclass[12pt]{article} \n\
\\\usepackage{mathtools} \n\
\\\usepackage{amsmath} \n\
\\\begin {document} \n\
\\\begin {enumerate} \n"

texEnd = "\\end {enumerate}\n\
\\\end {document}\n"

formulaHeader = "\\begin {equation*} \n\
\\\begin {gathered} \n"

formulaEnd = "\\end {gathered} \n\
\\\end {equation*} \n"

showListItem = ("\\item "++)
showArrow c = " \\xrightarrow{" ++ c : "} "
showVertex dfa v | isTerminal dfa v = "\\fbox{" ++ show v ++ "}"
                 | otherwise = show v


showPath :: DFA -> String -> [Int] -> String
showPath dfa chain vs = fst $ foldl f ini (zip chain (tail vs)) where
    maxLineLen = 150

    ini = (iniBlock, length iniBlock) where 
            iniBlock = showVertex dfa $ head vs

    f (st, lineLen) (c, v) | newLen <= maxLineLen = (st ++ block, newLen)
                           | otherwise = (st ++ arrow ++ "\\\\\n" ++ block, length block)
        where 
            block = arrow ++ (showVertex dfa v)
            newLen = lineLen + length block
            arrow = showArrow c


showTex dfa chains results = texHeader ++ (foldl f "" $ zip chains results) ++ texEnd where
    f st (chain, result) = 
        st 
        ++ (showListItem chain) ++ "\n"
        ++ formulaHeader
        ++ (showPath dfa chain result) ++ "\n"
        ++ formulaEnd
