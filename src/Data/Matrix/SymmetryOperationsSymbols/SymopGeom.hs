module Data.Matrix.SymmetryOperationsSymbols.SymopGeom where

import Data.Ratio
--import Data.Matrix.SymmetryOperationsSymbols.Common2

data ABC = A | B | C deriving (Show,Read,Eq)
data DGN = D | G | N deriving (Show,Read,Eq)
data NFold = ThreeFold | FourFold | SixFold deriving (Show,Read,Eq)
data Sense = Positive | Negative deriving (Show,Read,Eq)

data SymopGeom a
  = Identity
  | Translation     { vector :: (Ratio a,Ratio a,Ratio a) }
  | Reflection      { plane :: [[Ratio a]] }
  | GlideABC        { abc :: ABC, plane :: [[Ratio a]] }
  | GlideDGN        { dgn :: DGN, plane :: [[Ratio a]], glide :: (Ratio a,Ratio a,Ratio a) }
  | TwoFoldRotation { axis :: [[Ratio a]] }
  | TwoFoldScrew    { axis :: [[Ratio a]], vector :: (Ratio a,Ratio a,Ratio a) }
  | NFoldRotation   { nFold :: NFold, sense :: Sense, axis :: [[Ratio a]] }
  | NFoldScrew      { nFold :: NFold, sense :: Sense, axis :: [[Ratio a]], vector :: (Ratio a,Ratio a,Ratio a) }
  | Inversion       { centre :: (Ratio a,Ratio a,Ratio a) }
  | RotInversion    { nFold :: NFold, sense :: Sense, axis :: [[Ratio a]], point :: (Ratio a,Ratio a,Ratio a) }
  deriving (Read,Show,Eq)
