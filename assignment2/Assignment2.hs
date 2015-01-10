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
     deriving (Ord, Eq, Show)      -- TODO: remove "Show" and replace by instance of Show:
-- instance Show a => Show (KTree a) where    
--  show _ = ""                    -- TODO: implement



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
addNode _ _ = Empty 0  -- dummy... TODO: replace by implementation






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
shiftChar _ _ = 'A'   -- dummy... TODO: replace by implementation




--
-- 2.2
--


rotateStep :: Enigma -> Enigma
rotateStep _ = []     -- dummy... TODO: replace by implementation

 
--
-- 2.3
--


encryptChar :: Char -> State Enigma Char
encryptChar _ = error "Not implemented"  -- dummy... TODO: replace by implementation


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





