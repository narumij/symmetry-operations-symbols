module SymmetryOperationsSymbolsSpec where

import Test.Hspec
import Data.Matrix.SymmetryOperationsSymbols
import Data.Matrix.SymmetryOperationsSymbols.Parse

import Data.Matrix
import Data.Matrix.AsXYZ

import TestDataOthers
import TestDataHex

spec :: Spec
spec = do
   describe "Data.Matrix.SymmetryOperationsSymbols.toMatrix" $ do
     mapM_ (toMatrixTest toMatrix) $ zip [0..] testDataOthers

   describe "Data.Matrix.SymmetryOperationsSymbols.toMatrixHex" $ do
     mapM_ (toMatrixTest toMatrixHex) $ zip [0..] testDataHex

   describe "Data.Matrix.SymmetryOperationsSymbols.fromMatrix" $ do
     mapM_ (fromMatrixTest) $ zip [0..] testDataOthers

   describe "Data.Matrix.SymmetryOperationsSymbols.fromMatrix" $ do
     mapM_ (fromMatrixTest) $ zip [0..] testDataHex

mat34 = submatrix 1 3 1 4

toMatrixTest f (n,(a,b)) = it (show n ++ " : '" ++ b ++ "' -> '" ++ a ++ "'") $ do
  -- (prettyXYZ <$> f b) `shouldBe` (Just . prettyXYZ . fromXYZ $ a)
  (mat34 <$> f b) `shouldBe` (Just . mat34 . fromXYZ $ a)

fromMatrixTest (n,(a,b)) = it (show n ++ " : '" ++ a ++ "' -> '" ++ b ++ "'") $ do
  (parseSymmetryOperation <$> fromMatrix . fromXYZ $ a) `shouldBe` (parseSymmetryOperation b)
