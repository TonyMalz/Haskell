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
    show tree = printt 0 tree               

-- print a tree recursively
-- begin at root element
-- n -> starting number of indentations (increased by 1 every level)
printt :: (Show a) => Int -> KTree a -> String
printt n (Empty _) = ""
printt n (KTree _ a []) =  addSpaces n ++ "+ " ++ show a ++ "\n" 
printt n (KTree _ a (x:xs)) = addSpaces n ++ "+ " ++ show a  ++ "\n" ++ printt (n+1) x ++ (printtRec (n+1) xs)
    where printtRec n [] = "" 
          printtRec n (x:xs) = printt n x ++ printtRec n xs

-- generate n times padding-spaces for indentation
addSpaces :: Int -> String
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
addNode a tree@(KTree n b nodes@(x:xs))
    | n > length nodes = insertNode a tree
    | n == length nodes = if (treeHeight $ addNode a x) <= (maxHeight xs) + 1 
                          -- check if adding a node to the left-most sub-node would increase the sub-tree heigth by more than 1
                          then KTree n b (addNode a x : xs)
                          else KTree n b (x : [addNode a (head xs)] ++ (tail xs))
                          where maxHeight [] = 0
                                maxHeight (x:xs) = maximum $ [treeHeight x] ++ [maxHeight xs]

-- auxilary function to add a given Node to a given KTree
insertNode:: a -> KTree a -> KTree a
insertNode a (KTree n b nodes) = KTree n b (nodes ++ [KTree n a []])
insertNode a (Empty n ) = KTree n a []

--test = addNode 7 $ addNode 6 $ addNode 5 $ addNode 4 $ addNode 3 $ addNode 2 $ addNode 1 $ addNode 20 (KTree 2 5 [])


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
encryptMessage s = encryptMessageRec s ""
    where encryptMessageRec [] enc = State (\s -> (s, enc))
          encryptMessageRec (x:xs) enc = do y <- encryptChar x
                                            encryptMessageRec xs (enc ++ [y])
--
-- 2.5
--

mystMsg :: String
mystMsg = "MKWJGKQDRUTZCNFNKMPLQVIQGHIYDXVKORGZGTEPTGJNQYPKTVJTQCGUFHIAFZMAQM"


-- Decryption
-- -> reverse all encryption functions
rotateBackStep :: Enigma -> Enigma
rotateBackStep [] = []
rotateBackStep xs = if newPos == (length $ fst $ last xs) - 1 then (rotateBackStep $ init xs) ++ [(fst $ last xs, newPos)]
                else init xs ++  [(fst $ last xs, newPos)]
                where newPos = mod (-1 + (snd $ last xs)) (length $ fst $ last xs)

shiftBackChar :: Char -> Char -> Char
shiftBackChar char offset = if ordChar < 65 || ordChar > 90 || ordOff < 65 || ordOff > 90
                        then toUpper char
                        else chr ((mod ((ordChar - 65) - (ordOff - 64)) 26) + 65)
                        where ordChar = ord $ toUpper char
                              ordOff = ord $ toUpper offset

decryptChar :: Char -> State Enigma Char
decryptChar char = State (\s -> ( rotateBackStep s, shiftRec char (rotateBackStep s)))
    where shiftRec c [] = c
          shiftRec c (x:xs) = shiftRec (shiftBackChar c (fst x !! snd x )) xs

decryptMessage :: String -> State Enigma String
decryptMessage s = decryptMessageRec (reverse s) ""
    where decryptMessageRec [] enc = State (\s -> (s, enc))
          decryptMessageRec (x:xs) enc = do y <- decryptChar x
                                            decryptMessageRec xs ([y] ++ enc)

-- brute force:
-- 1. decrypt given message with every possible rotor position (20*20*20) and compare the first three characters
--    of the decoded string to start with NOM
-- 2. look at all possible results and determine which one is 'readable' or makes sense 
brutedecode = [ (decryptTest x y z, (x,y,z) )| x <- [0..19], y <-[0..19], z<-[0..19], take 3 (decryptTest x y z) == "NOM" ]

-- the only sensible decodable string out of 11 possibilities starting with NOM:
-- ("NOMILKTODAYMYLOVEHASGONEAWAYTHEBOTTLESTANDSFORLORNASYMBOLOFTHEDAWN",(17,5,18))
-- NO MILK TODAY MY LOVE HAS GONE AWAY THE BOTTLE STANDS FORLORN A SYMBOL OF THE DAWN (Herman's Hermits)
-- rotor 1: 17, rotor 2: 5, rotor 3: 18
-- shift rotors back by length of encoded string to get initial positions
-- rotateBack (length mystMsg ) 17 5 18
-- (17,2,12) -> Initial rotor positions:  rotor1: 17, rotor2: 2, rotor3: 12

-- get decrypted message by rotor position
decryptTest :: Int -> Int -> Int -> String
decryptTest rot1 rot2 rot3 = snd $ runState [(rotorExample1,rot1),(rotorExample2,rot2),(rotorExample3,rot3)] $ decryptMessage mystMsg

-- encrypt message by rotor position
encryptTest :: String -> Int -> Int -> Int -> String
encryptTest msg rot1 rot2 rot3 = snd $ runState [(rotorExample1,rot1),(rotorExample2,rot2),(rotorExample3,rot3)] $ encryptMessage msg

-- shift back rotors by n positions
rotateBack n rot1 rot2 rot3 = rotateBackRec n [(rotorExample1,rot1),(rotorExample2,rot2),(rotorExample3,rot3)]
    where rotateBackRec 0 ([(_,r1),(_,r2),(_,r3)]) = (r1,r2,r3)
          rotateBackRec n enigma = rotateBackRec (n-1) $ rotateBackStep enigma
