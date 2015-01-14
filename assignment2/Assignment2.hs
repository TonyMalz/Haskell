--
-- insert your name and matriculation number here
--


module Assignment2 where

import Data.List
import Data.Char


--
-- Task 1:
--

--
-- 1.1
--

data KTree a = KTree Int a [KTree a] | Empty Int
     deriving (Ord, Eq)

instance Show a => Show (KTree a) where    
    show n = printt 0 n               

printt :: (Show a) => Int -> KTree a -> String
printt n (Empty _) = ""
printt n (KTree _ a []) =  addSpaces n ++ "+ " ++ show a ++ "\n" 
printt n (KTree _ a (x:xs)) = addSpaces n ++ "+ " ++ show a  ++ "\n" ++ printt (n+1) x ++ (printtRec (n+1) xs)
    where printtRec n [] = "" 
          printtRec n (x:xs) = printt n x ++ printtRec n xs
addSpaces n = concat $ take n $ repeat "  "


countNodes :: KTree a -> Int
countNodes (Empty _) = 0
countNodes (KTree _ _ []) = 1 
countNodes (KTree _ _ nodes) = 1 + (sum $ map countNodes nodes)

treeHeight :: KTree a -> Int
treeHeight (Empty _)  =  0
treeHeight (KTree _ _ []) = 0
treeHeight (KTree _ _ nodes) = 1 + (maximum $ map treeHeight nodes)


--
-- 1.2
--

addNode :: a -> KTree a -> KTree a
addNode a tree@(KTree n b []) = insertNode a tree
addNode a tree@(Empty n) = insertNode a tree
addNode a tree@(KTree n b nodes)
    | n > length nodes = insertNode a tree
    | n == length nodes = if (treeHeight $ addNode a (head nodes)) <= (treeHeight $ head $ tail nodes) + 1
                          then KTree n b (addNode a (head nodes) : (tail nodes))
                          else KTree n b ((head nodes) : [addNode a ( head $ tail nodes)])

insertNode a (KTree n b nodes) = KTree n b (nodes ++ [KTree n a []])
insertNode a (Empty n ) = KTree n a []



-- pre-order traversal
traverse :: KTree a -> [a]
traverse (Empty _) = []
traverse (KTree _ x []) = [x]
traverse (KTree _ p (x:xs)) = (p : (traverse x)) ++ (traverseRec xs)
    where traverseRec [] = [] 
          traverseRec (x:xs) = traverse x ++ traverseRec xs



--
-- Task 2
--

data State s a = State (s -> (s,a))


instance Monad (State s) where
  return x = State $ \s -> (s,x)  
  (State h) >>= f = State $ \s -> let (newState, a) = h s  
                                      (State g) = f a  
                                    in g newState

runState :: s -> State s a -> (s, a)
runState init (State step) = step init



type Rotor = [Char]
type Position = Int
type Enigma = [(Rotor,Position)]


-- Constants for testing purposes:

rotorExample1 :: Rotor
rotorExample1 = "GJWQICNQOFRNPCQZYKIV"

rotorExample2 :: Rotor
rotorExample2 = "BAXIEWQLMLLSGTSPOONV"

rotorExample3 :: Rotor
rotorExample3 = "TZASJHWXLKUYPMARLQNF"


enigmaTest1 :: Enigma
enigmaTest1 = [("ABC",0),("RSM",0),("ZBX",0)]

enigmaTest2 :: Enigma
enigmaTest2 = [(rotorExample1,0),(rotorExample2,0),(rotorExample3,0)]

--
-- 2.1
--

shiftChar :: Char -> Char -> Char
shiftChar char offset = if ordChar < 65 || ordChar > 90 || ordOff < 65 || ordOff > 90
                        then toUpper char
                        else chr ((mod (ordOff + ordChar - 129) 26) + 65)
                        where ordChar = ord $ toUpper char
                              ordOff = ord $ toUpper offset



--
-- 2.2
--


--rotateStep :: Enigma -> Enigma
rotateStep :: Enigma -> Enigma
rotateStep [] = []
rotateStep xs = if newPos == 0 then (rotateStep $ init xs) ++ [(fst $ last xs, newPos)]
                else init xs ++  [(fst $ last xs, newPos)]
                where newPos = mod (1 + (snd $ last xs)) (length $ fst $ last xs)
--
-- 2.3
--



encryptChar :: Char -> State Enigma Char
encryptChar char = State (\s -> (rotateStep s, shiftRec char s))
    where shiftRec c [] = c
          shiftRec c (x:xs) = shiftRec (shiftChar c (fst x !! snd x )) xs
--
-- 2.4
--

encryptMessage :: String -> State Enigma String
encryptMessage _ = error "Not implemented" -- dummy... TODO: replace by implementation


--
-- 2.5
--

mystMsg :: String
mystMsg = "MKWJGKQDRUTZCNFNKMPLQVIQGHIYDXVKORGZGTEPTGJNQYPKTVJTQCGUFHIAFZMAQM"





