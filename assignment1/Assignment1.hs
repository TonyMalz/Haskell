
--
-- Enter name and matriculation number here!
--

module Assignment1 where

import Data.Char


--
-- Task 1.1
--


findPos :: String -> [String] -> Int
findPos s xs = (\xs -> if null xs then -1 else head xs) [i| (i,x)<-zip [0..] xs, map toLower s == map toLower x]



findCharStrings :: Char -> [String] -> [String]
findCharStrings c xs = filter (\xs -> any (\s -> s == toLower c) (map toLower xs) ) xs



--
-- Task 1.2
--

sortStrings :: [String] -> [String]
sortStrings []     = []
sortStrings (x:xs) = sort x (sortStrings xs)
    where sort x [] = [x]
          sort x sorted@(y:ys) | (map toLower x) < (map toLower y) = x : sorted
                               | otherwise = y : sort x ys


-- Simple implementation of insertion sort O(n^2)
-- take first element (x) of UNSORTED (xs) and insert it at the right position in already SORTED (sortStrings xs)
-- sort recursively shoves the biggest element to the right or rather at the end of the list


--
-- Task 1.3
--


nameScore :: String -> [String] -> Integer
nameScore _ [] = (-1)
nameScore a xs = let pos = fromIntegral(findPos a xs) in 
                    if pos > -1 then pos * fromIntegral(sum $ map (\c -> let diff=ord c - ord 'a' in if diff<0 || diff>25 then 0 else 1+diff  ) $ map toLower a)
                    else -1




--
-- Task 2.1
--


readConsole :: IO ()
readConsole = do
                putStrLn "Give a Number:"
                input <- getLine
                if (length $ filter isDigit input) == length input
                    then
                        putStrLn ("The digit-sum of "++ input ++ " is " ++ show (sumUp input))
                    else 
                        putStrLn ("Next time, enter a valid number!")
                where sumUp [] = 0
                      sumUp (x:xs) = ( (ord x) - 48) + sumUp xs  

-- Task 2.2
--


readFileTokens :: FilePath -> IO [String]
readFileTokens path = do 
                        content <- readFile path
                        return (words $ filter (\s -> isAlpha s || isSpace s ) content)



writeFileTokens :: [String] -> FilePath -> IO ()
writeFileTokens xs path = writeFile path (conc xs)
    where conc (x:xs) = if not $ null xs then x ++ ", " ++ conc xs else x
          conc _  = []

main :: IO ()
main = do
        putStrLn "Please enter the path to the input file:"
        inPath <- getLine
        contents <- readFile inPath 
        let tokens = words contents
        let sortedTokens = sortStrings tokens
        putStrLn "Please enter the path to the output file:"
        outPath <- getLine
        putStrLn "Writing to file..."
        -- calc
        writeFile outPath $ concat [ token ++ ", " ++ show score ++ "\n" |  token <- tokens, let score = nameScore token sortedTokens]





