module ParseSpec where

import Test.Hspec
import Control.Exception (evaluate)
import Text.ParserCombinators.Parsec
import Data.Matrix.SymmetryOperationsSymbols.Parser

parseSymmetryOperation' :: String -> Maybe (String,String,String,String)
parseSymmetryOperation' st = case parse (symmetryElement return) st st of
  Right a -> Just a
  Left _ -> Nothing

spec :: Spec
spec = do
  
   describe "Data.Matrix.SymmetryOperationsSymbols.Parse.parseSymmetryOperation" $ do

     it "parse ' 1 ' is just something" $ do
       parseSymmetryOperation' " 1 " `shouldBe` Just ("1","","","")

     it "parse ' -1 0,0,0' is just something" $ do
       parseSymmetryOperation' " -1 0,0,0" `shouldBe` Just ("-1","","0,0,0","")

     it "parse ' -1 0,0,0; 0,0,0' is nothing" $ do
       parseSymmetryOperation' " -1 0,0,0; 0,0,0" `shouldBe` Nothing

     it "parse ' -3- 0,0,0; 0,0,0' is just something" $ do
       parseSymmetryOperation' " -3- 0,0,0; 0,0,0" `shouldBe` Just ("-3","-","0,0,0","0,0,0")

     it "parse ' -3 0,0,0; 0,0,0' is nothing" $ do
       parseSymmetryOperation' " -2 0,0,0; 0,0,0" `shouldBe` Nothing

     it "parse ' -3 x,y,z; 0,0,0' is nothing" $ do
       parseSymmetryOperation' " -2 x,y,z; 0,0,0" `shouldBe` Nothing

     it "parse ' -3 0,0,0; x,y,z' is nothing" $ do
       parseSymmetryOperation' " -2 0,0,0; x,y,z" `shouldBe` Nothing

     it "parse ' 2 (0,0,0) x,y,z' is nothing" $ do
       parseSymmetryOperation' " 2 (0,0,0) x,y,z" `shouldBe` Just ("2","","0,0,0","x,y,z")

     it "parse ' 3+ (0,0,0) x,y,z ' is nothing" $ do
       parseSymmetryOperation' " 3+ (0,0,0) x,y,z" `shouldBe` Just ("3","+","0,0,0","x,y,z")

     it "parse ' 6- (0,0,0) x,y,z ' is nothing" $ do
       parseSymmetryOperation' " 6- (0,0,0) x,y,z " `shouldBe` Just ("6","-","0,0,0","x,y,z")

     it "parse ' 6- (x,y,z) x,y,z ' is nothing" $ do
       parseSymmetryOperation' " 6- (x,y,z) x,y,z " `shouldBe` Nothing

    -- 理想的にははじきたいけれど、現段階では保留
     -- it "parse ' 6- (0,0,0) 0,0,0 ' is nothing" $ do
     --   parseSymmetryOperation " 6- (0,0,0) 0,0,0 " `shouldBe` Nothing

     it "parse ' g (0,0,0) x,y,z ' is nothing" $ do
       parseSymmetryOperation' " g (0,0,0) x,y,z " `shouldBe` Just ("g","","0,0,0","x,y,z")

     it "parse ' g ( 0, 0, 0 )  x, y, z ' is nothing" $ do
       parseSymmetryOperation' " g ( 0, 0, 0 )  x, y, z " `shouldBe` Just ("g","","0,0,0","x,y,z")

     it "parse ' m x,y,z ' is nothing" $ do
       parseSymmetryOperation' " m x,y,z " `shouldBe` Just ("m","","","x,y,z")

     it "parse ' a x,y,z ' is nothing" $ do
       parseSymmetryOperation' " a x,y,z " `shouldBe` Just ("a","","","x,y,z")

     it "parse ' m (0,0,0) x,y,z ' is nothing" $ do
       parseSymmetryOperation' " m (0,0,0) x,y,z " `shouldBe` Nothing

     it "parse ' a (0,0,0) x,y,z ' is nothing" $ do
       parseSymmetryOperation' " a (0,0,0) x,y,z " `shouldBe` Nothing
