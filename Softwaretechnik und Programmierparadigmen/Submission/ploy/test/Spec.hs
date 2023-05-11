-- #############################################################################
-- ########### YOUR UNIT TESTS                                    ##############
-- ########### Note: execute tests using "stack test ploy:units"  ##############
-- #############################################################################
import Test.Hspec
import Board
import Ploy



main :: IO ()
main = hspec $ do
    testValidateFEN
    testBuildBoard
    testLine
    testGameFinished
    testIsValidMove
    testPossibleMoves
    testListMoves
    testShow
    testRotate
    testConstroctor

testShow :: Spec
testShow =
    describe "Show" $ do
        it "Show Pos" $ 
            show (Pos 'a' 1) `shouldBe` "Pos {col = 'a', row = 1}"
        it "Show Move" $ 
            show (Move (Pos 'e' 3) (Pos 'e' 3) 7) `shouldBe` ("e3-e3-7")
        it "Show Cell" $ 
            show (Empty) `shouldBe` ("Empty")
        it "Show Player" $ 
            show (Black) `shouldBe` ("Black")
        it "start" $ do
            show (start(Move (Pos 'a' 1) (Pos 'a' 1) 1)) `shouldBe` ("Pos {col = 'a', row = 1}"::String)
        it "target" $ do
            show (target(Move (Pos 'a' 1) (Pos 'a' 1) 1)) `shouldBe` ("Pos {col = 'a', row = 1}"::String)
        it "turn test" $ do
            show (turn(Move (Pos 'a' 1)(Pos 'a' 1)1)) `shouldBe` ("1"::String)


testRotate :: Spec
testRotate =
    describe "Rotate" $ do
        it "Rotate 1" $ 
            rotate 1 2 `shouldBe` (4 :: Int)

testConstroctor:: Spec
testConstroctor =
    describe "Constructor" $ do
        it "Constructor 1" $ 
            (Pos 'a' 1) `shouldBe` (Pos 'a' 1 :: Pos)
        it "Constructor 2" $ 
            (Move (Pos 'a' 1) (Pos 'a' 1) 1) `shouldBe` (Move (Pos 'a' 1) (Pos 'a' 1) 1 :: Move)
        it "Constructor 3" $ 
            (Piece Black 3) `shouldBe` (Piece Black 3 :: Cell)
        it "Constructor 4" $ 
            (Empty) `shouldBe` (Empty :: Cell)
        it "start" $
            show (start(Move (Pos 'a' 1) (Pos 'a' 1) 0)) `shouldBe` ("Pos {col = 'a', row = 1}"::String)
        it "target" $
            show (target(Move (Pos 'a' 1) (Pos 'a' 1)0)) `shouldBe` ("Pos {col = 'a', row = 1}"::String)
        it "turn" $
            show (turn(Move (Pos 'a' 1) (Pos 'a' 1) 0)) `shouldBe` ("0"::String)
            
        
        

testValidateFEN :: Spec
testValidateFEN =
    describe "ValidateFEN" $ do
        it "Normal String" $ 
            validateFEN (",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (True :: Bool) 
        it "Wrong Number" $ 
            validateFEN (",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w300,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool) 
        it "extra comma" $ 
            validateFEN (",,w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool) 
        it "missing comma" $ 
            validateFEN ("w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool) 
        it "extra Row" $ 
            validateFEN (",w84,w41,w56,w170,w56,w41,w84,/,w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool) 
        it "Missing Row" $ 
            validateFEN (",,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool) 
        it "wrong letter" $ 
            validateFEN (",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,j16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool) 
        it "wrong string" $ 
            validateFEN (",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,abc,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool) 
        it "wrong Number2" $ 
            validateFEN (",w84,w41,w56,w170,w56,w-42,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,j16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,") `shouldBe` (False :: Bool) 

testBuildBoard :: Spec 
testBuildBoard = 
    describe "BuildBoard" $ do
        it "Normal Board" $
            buildBoard (",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,")  `shouldBe`
            [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]
        it "one Piece is Empty and one Empty is Piece" $
            buildBoard (",,w41,w56,w170,w56,w41,w84,w84/,,w24,w40,w17,w40,w48,,/,,,w16,w16,w16,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,b1,b1,b1,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,")  `shouldBe`[[Empty,Empty,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Piece White 84],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]
        it "a lot of Empty" $
            buildBoard (",w84,w41,w56,w170,w56,w41,w84,/,,w24,w40,w17,w40,w48,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,,,,,,,/,,b3,b130,b17,b130,b129,,/,b69,b146,b131,b170,b131,b146,b69,")  `shouldBe`[[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]

testLine:: Spec
testLine = 
    describe "Line" $ do 
        it "same" $
          line (Pos 'a' 1) (Pos 'a' 1) `shouldBe` [(Pos 'a' 1)]
        it "horizental forward" $
          line (Pos 'a' 1) (Pos 'd' 1) `shouldBe` [(Pos 'a' 1),(Pos 'b' 1),(Pos 'c' 1),(Pos 'd' 1)]
        it "horizental backward" $
          line (Pos 'd' 1) (Pos 'a' 1) `shouldBe` [(Pos 'd' 1),(Pos 'c' 1),(Pos 'b' 1),(Pos 'a' 1)]
        it "vertical forward" $
          line (Pos 'a' 1) (Pos 'a' 4) `shouldBe` [(Pos 'a' 1),(Pos 'a' 2),(Pos 'a' 3),(Pos 'a' 4)]
        it "vertical backward" $
          line (Pos 'a' 4) (Pos 'a' 1) `shouldBe` [(Pos 'a' 4),(Pos 'a' 3),(Pos 'a' 2),(Pos 'a' 1)]
        it "first diagonal" $
          line (Pos 'a' 1) (Pos 'd' 4) `shouldBe` [(Pos 'a' 1),(Pos 'b' 2),(Pos 'c' 3),(Pos 'd' 4)]
        it "second diagonal" $
          line (Pos 'a' 4) (Pos 'd' 1) `shouldBe` [(Pos 'a' 4),(Pos 'b' 3),(Pos 'c' 2),(Pos 'd' 1)]
        it "third diagonal" $
          line (Pos 'd' 4) (Pos 'a' 1) `shouldBe` [(Pos 'd' 4),(Pos 'c' 3),(Pos 'b' 2),(Pos 'a' 1)]
        it "fourth diagonal" $
          line (Pos 'd' 1) (Pos 'a' 4) `shouldBe` [(Pos 'd' 1),(Pos 'c' 2),(Pos 'b' 3),(Pos 'a' 4)]

testGameFinished :: Spec
testGameFinished =
    describe "Gamefinished" $ do
        it "not finished: begining" $
            gameFinished ([[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]) `shouldBe` (False :: Bool)
        it "not finished: White still have Comm and one Piece" $
            gameFinished ([[Empty,Empty,Empty,Empty,Piece White 170,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]) `shouldBe` (False :: Bool)
        it "not finished: Black still have Comm and one Piece" $
            gameFinished ([[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 129,Empty,Empty],[Empty,Empty,Empty,Empty,Piece Black 85,Empty,Empty,Empty,Empty]]) `shouldBe` (False :: Bool)
        it "finished: one White Comm" $
            gameFinished ([[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 129,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]) `shouldBe` (True :: Bool)
        it "finished: one Black Comm" $
            gameFinished ([[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]) `shouldBe` (True :: Bool)
        it "finished: Two Comm , Black one Piece" $
            gameFinished ([[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Piece Black 85,Empty,Empty,Empty,Empty]]) `shouldBe` (True :: Bool)
        it "finished: Two Comm , White one Piece" $
            gameFinished ([[Empty,Empty,Empty,Empty,Piece White 85,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]]) `shouldBe` (True :: Bool)
   
testIsValidMove :: Spec
testIsValidMove = 
    describe "isValidMove" $ do
        it "the start is Empty" $
            isValidMove [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] (Move (Pos 'a' 1) (Pos 'a' 2) 0)`shouldBe` (False :: Bool)
        it "the middle is not Empty" $
            isValidMove [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] (Move (Pos 'd' 1) (Pos 'd' 4) 0)`shouldBe` (False :: Bool)
        it "the goal is same color" $
            isValidMove [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] (Move (Pos 'c' 8) (Pos 'd' 7) 0)`shouldBe` (False :: Bool)
        it "rotate wrong" $
            isValidMove [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] (Move (Pos 'h' 0) (Pos 'h' 1) 10)`shouldBe` (False :: Bool)
        it "out of Board" $
            isValidMove [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] (Move (Pos 'h' 0) (Pos 'h' 1) 6)`shouldBe` (False :: Bool)
        it "Good one 1" $
            isValidMove [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] (Move (Pos 'e' 3) (Pos 'e' 4) 2)`shouldBe` (True :: Bool)
        it "Good one 2" $
            isValidMove [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] (Move (Pos 'd' 2) (Pos 'b' 4) 0)`shouldBe` (True :: Bool)
        it "Good one 3" $
            isValidMove [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] (Move (Pos 'h' 1) (Pos 'h' 4) 0)`shouldBe` (True :: Bool)
        it "Good one 4" $
            isValidMove [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] (Move (Pos 'h' 1) (Pos 'h' 1) 6)`shouldBe` (True :: Bool)
        it "Good one 5" $
            isValidMove [[Piece White 84,Empty,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Piece Black 69,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Empty,Empty]] (Move (Pos 'a' 9) (Pos 'a' 7) 0)`shouldBe` (True :: Bool)

testPossibleMoves :: Spec
testPossibleMoves = 
        describe "possibleMoves" $ do
            it "Corner 1" $
                possibleMoves (Pos 'a' 1) (Piece Black 69 ) `shouldContain` ([Move (Pos 'a' 1) (Pos 'a' 2) 0] :: [Move])
            it "Corner 2" $
                possibleMoves (Pos 'a' 1) (Piece Black 69 ) `shouldContain` ([Move (Pos 'a' 1) (Pos 'a' 3) 0] :: [Move])
            it "Corner 3" $
                possibleMoves (Pos 'a' 1) (Piece Black 69 ) `shouldContain` ([Move (Pos 'a' 1) (Pos 'a' 4) 0] :: [Move])
            it "Corner turn " $
                possibleMoves (Pos 'a' 1) (Piece Black 69 ) `shouldContain` ([Move (Pos 'a' 1) (Pos 'a' 1) 7] :: [Move])
            it "shield move Turn" $
                possibleMoves (Pos 'e' 3) (Piece Black 1 ) `shouldContain` ([Move (Pos 'e' 3) (Pos 'e' 4) 7] :: [Move])
            it "Comm move " $
                possibleMoves (Pos 'e' 1) (Piece Black 170 ) `shouldContain` ([Move (Pos 'e' 1) (Pos 'f' 2) 0] :: [Move])
            it "Comm Turn" $
                possibleMoves (Pos 'e' 3) (Piece Black 170 ) `shouldContain` ([Move (Pos 'e' 3) (Pos 'e' 3) 7] :: [Move])
            it "Probe Turn" $
                possibleMoves (Pos 'd' 8) (Piece White 40 ) `shouldContain` ([Move (Pos 'd' 8) (Pos 'd' 8) 7] :: [Move])
            it "Probe move" $
                possibleMoves (Pos 'd' 8) (Piece White 40 ) `shouldContain` ([Move (Pos 'd' 8) (Pos 'e' 7) 0] :: [Move])
            it "Special Probe Turn1" $
                possibleMoves (Pos 'e' 2) (Piece Black 17 ) `shouldContain` ([Move (Pos 'e' 2) (Pos 'e' 2) 7] :: [Move])
            it "Special Probe Turn2" $
                possibleMoves (Pos 'e' 8) (Piece White 68 ) `shouldContain` ([Move (Pos 'e' 8) (Pos 'e' 8) 1] :: [Move])  
            it "Lance move" $
                possibleMoves (Pos 'c' 9) (Piece White 41 ) `shouldContain` ([Move (Pos 'c' 9) (Pos 'd' 8) 0] :: [Move])
            it "Lance Turn" $
                possibleMoves (Pos 'b' 9) (Piece White 41 ) `shouldContain` ([Move (Pos 'b' 9) (Pos 'b' 9) 4] :: [Move])

testListMoves :: Spec
testListMoves = 
        describe "listMoves" $ do
            it "finished" $
                listMoves ([[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Piece Black 129,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]]) Black `shouldBe` []
            it "Lance1 Move" $
                listMoves [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] Black `shouldContain` ([Move (Pos 'b' 1) (Pos 'b' 4) 0] :: [Move]) 
            it "Lance1 Turn" $
                listMoves [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] Black `shouldContain` ([Move (Pos 'b' 1) (Pos 'b' 1) 1] :: [Move]) 
            it "Probe1 Move" $
                listMoves [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] White `shouldContain` ([Move (Pos 'g' 8) (Pos 'g' 6) 0] :: [Move]) 
            it "Probe1 Turn" $
                listMoves [[Empty,Piece White 84,Piece White 41,Piece White 56,Piece White 170,Piece White 56,Piece White 41,Piece White 84,Empty],[Empty,Empty,Piece White 24,Piece White 40,Piece White 17,Piece White 40,Piece White 48,Empty,Empty],[Empty,Empty,Empty,Piece White 16,Piece White 16,Piece White 16,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty],[Empty,Empty,Empty,Piece Black 1,Piece Black 1,Piece Black 1,Empty,Empty,Empty],[Empty,Empty,Piece Black 3,Piece Black 130,Piece Black 17,Piece Black 130,Piece Black 129,Empty,Empty],[Empty,Piece Black 69,Piece Black 146,Piece Black 131,Piece Black 170,Piece Black 131,Piece Black 146,Piece Black 69,Empty]] White `shouldContain` ([Move (Pos 'g' 8) (Pos 'g' 8) 4] :: [Move]) 
            
         
