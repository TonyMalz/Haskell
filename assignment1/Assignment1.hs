
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
-- take first element (x) of unsorted (xs) and insert it at the right position in sorted
-- repeat until unsorted is empty



--
-- Task 1.3
--


nameScore :: String -> [String] -> Integer
nameScore _ _ = 0   -- DUMMY: replace by implementation




--
-- Task 2.1
--


readConsole :: IO ()
readConsole = return ()   -- DUMMY: replace by implementation




--
-- Task 2.2
--


readFileTokens :: FilePath -> IO [String]
readFileTokens _ = return []   -- DUMMY: replace by implementation



writeFileTokens :: [String] -> FilePath -> IO ()
writeFileTokens _ _ = return ()   -- DUMMY: replace by implementation



main :: IO ()
main = return ()   -- DUMMY: replace by implementation





