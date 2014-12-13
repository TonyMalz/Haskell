
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
sortStrings (x:xs) = insertSorted x (sortStrings xs)
    where insertSorted x []     = [x]
          insertSorted x (y:ys) = if (map toLower x) < (map toLower y) then (x:y:ys) else y:(insertSorted x ys)


-- Simple implementation of insertion sort O(n^2)
-- take first element (x) of UNSORTED (xs) and insert it at the right position in already SORTED (sortStrings xs)
-- insertSorted recursively shoves the biggest element to the right or rather at the end of the list


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
main = return ()   -- DUMMY: replace by implementation





