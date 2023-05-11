module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
import Data.Char (isDigit,ord,chr)
import Data.List.Split
import Data.List (isPrefixOf)

-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Black | White deriving Show
data Cell = Piece Player Int | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Black Black = True
  (==) White White = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Piece p1 i1) (Piece p2 i2) = p1 == p2 && i1 == i2 
  (==) _ _ = False

-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################

deleteNum :: String -> String 
deleteNum [] = []
deleteNum (x:xs) = if isDigit x == True then deleteNum xs else x : deleteNum xs 

keepNum :: String -> String 
keepNum [] = []
keepNum (x:xs) = if isDigit x == False then keepNum xs else x : keepNum xs 

checkLetter:: String -> Bool 
checkLetter "" = True
checkLetter s = (length (deleteNum s) > 0 ) && (if deleteNum s == "b" || deleteNum s == "w" then True else False )

checkInt:: String -> Bool
checkInt "" = True
checkInt s = (length (keepNum s) > 0 ) && (if ((read (keepNum s)) > 0) &&  ((read (keepNum s)) < 256) then True else False)

afterkomma:: [String]  -> Bool
afterkomma [] = True
afterkomma (x:xs) = (checkInt x) && (checkLetter x) && afterkomma xs 

afterslash:: [String] -> Bool 
afterslash [] = True
afterslash (x:xs) = afterkomma (splitOn "," x) && (( length (splitOn "," x)) == 9) && afterslash xs

validateFEN :: String -> Bool
validateFEN s =  afterslash (splitOn "/" s) && (( length (splitOn "/" s)) == 9)

-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

makePlayer:: String -> (Player, Int) 
makePlayer s = (if (deleteNum s) == "w" then White else Black , read (keepNum s))

makeRow:: [String] -> [Cell]
makeRow [] = []
makeRow (x:xs) = if x == "" then Empty : makeRow xs else Piece (fst (makePlayer x)) (snd (makePlayer x)) :  makeRow xs

makeBoard:: [String] -> Board
makeBoard [] = []
makeBoard (x:xs) = makeRow (splitOn "," x) : makeBoard xs

buildBoard :: String -> Board
buildBoard s = makeBoard (splitOn "/" s)

-- #############################################################################
-- ####################### line :: Pos -> Pos -> [Pos]  ########################
-- ####################### - 3 Functional Points        ########################
-- ####################### - 1 Coverage Point           ########################
-- #############################################################################

difAsList:: Int -> Int -> [Int]
difAsList a b = if a < b then [a .. b] else reverse [b .. a]

makeList :: [Int] -> [Int] -> [Pos]
makeList [] _ = []
makeList _ [] = []
makeList l1 l2  | length l1 == 1 = let l1new = take (length l2) (cycle l1) 
                                        in (Pos { col = chr( head l1new ), row = head l2 }) : (makeList (tail l1new) (tail l2))
                | length l2 == 1 = let l2new = take (length l1) (cycle l2) 
                                        in (Pos { col = chr( head l1 ), row = head l2new }) : (makeList (tail l1) (tail l2new)) 
                | otherwise = Pos { col = chr( head l1 ), row = head l2 } : (makeList (tail l1) (tail l2)) 


line :: Pos -> Pos -> [Pos]
line (Pos c a) (Pos d b ) = makeList (difAsList (ord c) (ord d) ) (difAsList a b)

-- ###########################
