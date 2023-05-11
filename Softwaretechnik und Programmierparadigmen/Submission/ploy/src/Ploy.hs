module Ploy where  -- do NOT CHANGE export of module

import Board

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
--       import Data.Char
import Data.Bits ( (.&.), (.|.), shift )
import Data.Char (isDigit,ord,chr)
import Data.List.Split
import Data.List 
import Data.Bits (popCount,testBit)


-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, turn :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ (show startR) ++ "-" ++ [tarC] ++ show tarR ++ "-" ++ show tr

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2 

rotate :: Int -> Int -> Int
rotate o tr = (.&.) ((.|.) (shift o tr) (shift o (tr-8))) 255



-- #############################################################################
-- ####################### gameFinished :: Board -> Bool #######################
-- ####################### - 3 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

isComm :: Cell -> Bool
isComm Empty = False
isComm (Piece Black a) =  a == 85 || a == 170 
isComm (Piece White a )=  a == 85 || a == 170 

numOfComm :: [Cell] -> Int
numOfComm [] = 0
numOfComm (x:xs) = if (isComm x) then 1 + numOfComm xs else numOfComm xs

isWhite:: Cell -> Bool
isWhite Empty = False
isWhite (Piece White a ) =  True
isWhite (Piece Black a ) = False


isBlack:: Cell -> Bool
isBlack Empty = False
isBlack (Piece Black a ) = True
isBlack (Piece White a )= False

numOfPieceBlack :: [Cell]  -> Int
numOfPieceBlack [] = 0
numOfPieceBlack (x:xs) = if (isBlack x) then 1 + numOfPieceBlack xs else numOfPieceBlack xs

numOfPieceWhite :: [Cell] -> Int
numOfPieceWhite [] = 0
numOfPieceWhite (x:xs) = if (isWhite x) then 1 + numOfPieceWhite xs else numOfPieceWhite xs

sumB:: Board -> Int
sumB [] = 0
sumB (x:xs) = numOfPieceBlack x + sumB xs

sumW:: Board -> Int
sumW [] = 0
sumW (x:xs) = numOfPieceWhite x + sumW xs

sumC::Board -> Int
sumC [] = 0
sumC (x:xs) = numOfComm x + sumC xs

gameFinished :: Board -> Bool
gameFinished b =( sumC b == 1 ) ||  (sumB b == 1) || (sumW b == 1) 

-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Functional Points                ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################

positionClean:: Board -> Move -> Bool
positionClean b (Move (Pos c1 r1) (Pos c2 r2) t ) = let (( Piece p1 a1),( Piece  p2 a2)) = (((b!!(9-r1))!!((ord c1) - 97)) , ((b!!(9-r2))!!((ord c2) - 97)))
                                                    in p1 /= p2

refactorLine:: Move -> [Pos]
refactorLine (Move p1 p2 t)  =  init(tail(line p1 p2)) 

middleClean:: Board -> [Pos] -> Bool 
middleClean b [] = True
middleClean b ((Pos c1 r1):xs) =  (((b!!(9-r1))!!((ord c1) - 97)) == ( Empty)) && (middleClean b xs)

roadClean:: Board -> Move -> Bool 
roadClean b (Move (Pos c1 r1) (Pos c2 r2) t ) | (Pos c1 r1) == (Pos c2 r2) = True
                                              |((b!!(9-r1))!!((ord c1) - 97)) /= Empty && ((b!!(9-r2))!!((ord c2) - 97)) == Empty = middleClean b (refactorLine (Move (Pos c1 r1) (Pos c2 r2)t))
                                              |((b!!(9-r1))!!((ord c1) - 97)) /= Empty && ((b!!(9-r2))!!((ord c2) - 97)) /= Empty = (positionClean b ( Move (Pos c1 r1) (Pos c2 r2)t)) 
                                                && (middleClean b (refactorLine (Move (Pos c1 r1) (Pos c2 r2) t)))
                                              | otherwise = False

isValidMove :: Board -> Move -> Bool
isValidMove b (Move (Pos c1 r1) (Pos c2 r2) t ) | (c1 > 'i') || (c1 < 'a') || (r1 > 9) || (r1 < 1) || (c2 > 'i') || (c2 < 'a') || (r2 > 9) || (r2 < 1) || (t > 7) || (t < 0)  == True = False
                                                | otherwise = roadClean b  (Move (Pos c1 r1) (Pos c2 r2) t )

                                                
-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 6 Functional Points                  ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################

allAround:: Pos -> Int ->Int -> [Pos] 
allAround (Pos c r ) 0 0 = []
allAround (Pos c r ) i j = let a = (Pos (chr((ord c) + i)) (r + j)) 
                    in a : allAround (Pos c r) (if i > 0 then i-1 else if i == 0 then 0 else i +1) (if j > 0 then j-1 else if j == 0 then 0 else j+1) 

notInBoard:: Pos -> Bool 
notInBoard(Pos c r ) = if (c <= 'i') && (c >= 'a') && (r <= 9) && (r >= 1) then True else False

directionGiver:: Int -> (Int,Int)
directionGiver d  | d == 0 = (0,3)
                  | d == 1 = (3,3)
                  | d == 2 = (3,0)
                  | d == 3 = (3,-3)
                  | d == 4 = (0,-3)
                  | d == 5 = (-3,-3)
                  | d == 6 = (-3,0)
                  | d == 7 = (-3,3)


whereOnes:: Int -> Int -> [Int]
whereOnes n (-1) = []
whereOnes n p = if (testBit n p) == True then p:(whereOnes n (p-1)) else []++(whereOnes n (p-1))

dirToList:: Pos ->[Int] -> [Pos]
dirToList _ [] = []
dirToList p (x:xs) = (allAround p (fst(directionGiver x)) (snd(directionGiver x))) ++ (dirToList p xs)

pacePiece:: Pos -> Pos ->Int -> Bool
pacePiece p1 p2 i = length (line p1 p2) <= i 

filterPiece:: [Pos] -> Pos -> Int -> [Pos] 
filterPiece [] _ _  = []
filterPiece (x:xs) p1 i = if (pacePiece x p1 i) == True then (x: filterPiece xs p1 i) else ([]++filterPiece xs p1 i)

combinePos:: Pos -> [Pos] -> Int -> [Move]
combinePos _ [] _ = []
combinePos p1 (x:xs) t = (Move p1 x t) : (combinePos p1 xs t)


onlyTurn:: Pos -> Int -> Bool -> Bool -> [Move]
onlyTurn p1 t damnComm damnProbe  | t<=0 =  []
                                  | (damnProbe == True) && (t /=4) =  (Move p1 p1 t) : (onlyTurn p1 (t-1) damnComm damnProbe )
                                  | (damnProbe == True) && (t == 4) =  [] ++ (onlyTurn p1 (t-1) damnComm damnProbe ) 
                                  | damnComm == True  =  (Move p1 p1 t) : (onlyTurn p1 (t-2) damnComm damnProbe )
                                  | otherwise = (Move p1 p1 t) : (onlyTurn p1 (t-1) damnComm damnProbe ) 

possibleMoves :: Pos -> Cell -> [Move]
possibleMoves p1 (Piece play n) | (popCount n) == 1 = combinePos p1 (filterPiece (filter notInBoard (dirToList p1 (whereOnes n 7))) p1 2) 0 ++
                                                      combinePos p1 (filterPiece (filter notInBoard (dirToList p1 (whereOnes n 7))) p1 2) 1 ++
                                                      combinePos p1 (filterPiece (filter notInBoard (dirToList p1 (whereOnes n 7))) p1 2) 2 ++
                                                      combinePos p1 (filterPiece (filter notInBoard (dirToList p1 (whereOnes n 7))) p1 2) 3 ++
                                                      combinePos p1 (filterPiece (filter notInBoard (dirToList p1 (whereOnes n 7))) p1 2) 4 ++
                                                      combinePos p1 (filterPiece (filter notInBoard (dirToList p1 (whereOnes n 7))) p1 2) 5 ++
                                                      combinePos p1 (filterPiece (filter notInBoard (dirToList p1 (whereOnes n 7))) p1 2) 6 ++
                                                      combinePos p1 (filterPiece (filter notInBoard (dirToList p1 (whereOnes n 7))) p1 2) 7 ++
                                                      (onlyTurn p1 7 False False)
                                | (popCount n) == 2 = combinePos p1 (filterPiece (filter notInBoard (dirToList p1 (whereOnes n 7))) p1 3) 0 ++
                                                      (onlyTurn p1 7 False (n==136 || n==68 || n==34 || n==17))
                                | (popCount n) == 3 = combinePos p1 (filterPiece (filter notInBoard (dirToList p1 (whereOnes n 7))) p1 4) 0 ++
                                                      (onlyTurn p1 7 False False)
                                | (popCount n) == 4 = combinePos p1 (filterPiece (filter notInBoard (dirToList p1 (whereOnes n 7))) p1 2) 0 ++
                                                      (onlyTurn p1 7 True False)
 

                                  



-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Functional Points                            ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################

                
areTheyValid:: Board -> [Move] -> [Move]
areTheyValid _ [] = []
areTheyValid b (x:xs) = if (isValidMove b x) == True then x:(areTheyValid b xs)  else [] ++ (areTheyValid b xs)


cellIsClean:: Board -> Player -> Pos -> Bool
cellIsClean b p (Pos c r) = (not(((b!!(9-r))!!((ord c) - 97)) == Empty)) && (let (Piece p2 n) = ((b!!(9-r))!!((ord c) - 97))
                            in  p == p2)
                          

posToMove:: Board-> Player -> Pos -> [Move] 
posToMove b p (Pos c r) = if cellIsClean b p (Pos c r) == False then []
                     else possibleMoves (Pos c r) ((b!!(9-r))!!((ord c) - 97)) 


monster:: Board -> Player -> Char -> Int -> [Move] 
monster b p c r  | c > 'i' && r <= 9 = [] ++ monster b p 'a' (r+1) 
                 | c <= 'i' && r <= 9 = (areTheyValid b (posToMove b p (Pos c r)))  ++ monster b p (chr((ord c)+1)) r 
                 | otherwise = []
            

listMoves :: Board -> Player -> [Move]
listMoves b p   = if  gameFinished b == True then []  else  (monster b p 'a' 1)
              
