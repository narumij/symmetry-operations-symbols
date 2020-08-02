{-# LANGUAGE FlexibleInstances #-}

module Data.Matrix.SymmetryOperationsSymbols.SymmetryOperation (
    SymmetryOperation(..),
    NFold(..),
    Sense(..),
    ABC(..),
    DGN(..),
    showSymmetryOperation,
    fromSymmetryOperation,
    fromSymop,
    )
    where

import Control.Monad

import Data.Ratio
import Data.Matrix
import Data.Matrix.AsXYZ
import Data.Matrix.SymmetryOperationsSymbols.Common
import Data.Matrix.SymmetryOperationsSymbols.Common2
import Data.Matrix.SymmetryOperationsSymbols.SymopGeom (ABC(..),DGN(..),NFold(..),Sense(..))
import qualified Data.Matrix.SymmetryOperationsSymbols.SymopGeom as S

fromSymmetryOperation :: SymmetryOperation a -> S.SymopGeom a
fromSymmetryOperation Identity = S.Identity
fromSymmetryOperation (Translation a) = S.Translation a
fromSymmetryOperation (Reflection a) = S.Reflection (toLists a)
fromSymmetryOperation (GlideABC a b) = S.GlideABC a (toLists b)
fromSymmetryOperation (GlideDGN a b c) = S.GlideDGN a (toLists b) c
fromSymmetryOperation (TwoFoldRotation a) = S.TwoFoldRotation (toLists a)
fromSymmetryOperation (TwoFoldScrew a b) = S.TwoFoldScrew (toLists a) b
fromSymmetryOperation (NFoldRotation a b c) = S.NFoldRotation a b (toLists c)
fromSymmetryOperation (NFoldScrew a b c d) = S.NFoldScrew a b (toLists c) d
fromSymmetryOperation (Inversion a) = S.Inversion a
fromSymmetryOperation (RotInversion a b c d) = S.RotInversion a b (toLists c) d

fromSymop :: S.SymopGeom a -> SymmetryOperation a
fromSymop S.Identity = Identity
fromSymop (S.Translation a) = Translation a
fromSymop (S.Reflection a) = Reflection (fromLists a)
fromSymop (S.GlideABC a b) = GlideABC a (fromLists b)
fromSymop (S.GlideDGN a b c) = GlideDGN a (fromLists b) c
fromSymop (S.TwoFoldRotation a) = TwoFoldRotation (fromLists a)
fromSymop (S.TwoFoldScrew a b) = TwoFoldScrew (fromLists a) b
fromSymop (S.NFoldRotation a b c) = NFoldRotation a b (fromLists c)
fromSymop (S.NFoldScrew a b c d) = NFoldScrew a b (fromLists c) d
fromSymop (S.Inversion a) = Inversion a
fromSymop (S.RotInversion a b c d) = RotInversion a b (fromLists c) d

data SymmetryOperation a
  = Identity
  | Translation     { vector :: (Ratio a,Ratio a,Ratio a) }
  | Reflection      { plane :: Matrix (Ratio a) }
  | GlideABC        { abc :: ABC, plane :: Matrix (Ratio a) }
  | GlideDGN        { dgn :: DGN, plane :: Matrix (Ratio a), glide :: (Ratio a,Ratio a,Ratio a) }
  | TwoFoldRotation { axis :: Matrix (Ratio a) }
  | TwoFoldScrew    { axis :: Matrix (Ratio a), vector :: (Ratio a,Ratio a,Ratio a) }
  | NFoldRotation   { nFold :: NFold, sense :: Sense, axis :: Matrix (Ratio a) }
  | NFoldScrew      { nFold :: NFold, sense :: Sense, axis :: Matrix (Ratio a), vector :: (Ratio a,Ratio a,Ratio a) }
  | Inversion       { centre :: (Ratio a,Ratio a,Ratio a) }
  | RotInversion    { nFold :: NFold, sense :: Sense, axis :: Matrix (Ratio a), point :: (Ratio a,Ratio a,Ratio a) }
  deriving (Eq)

instance (Integral a,Read a) => Read (SymmetryOperation a) where
  readsPrec n s = do
    (symop,st) <- reads s
    return (fromSymop symop, st)

instance (Integral a,Show a) => Show (SymmetryOperation a) where
  showsPrec n s = do
    shows (fromSymmetryOperation s)


showSymbol :: NFold -> String
showSymbol ThreeFold = "3"
showSymbol FourFold  = "4"
showSymbol SixFold   = "6"

showSense :: Sense -> String
showSense Positive = "+"
showSense Negative = "-"

showABC :: ABC -> String
showABC A = "a"
showABC B = "b"
showABC C = "c"

showDGN :: DGN -> String
showDGN D = "d"
showDGN G = "g"
showDGN N = "n"

showSymmetryOperation :: Integral a => SymmetryOperation a -> String
showSymmetryOperation Identity = " 1 "
showSymmetryOperation val@Translation {}
  = concat [" t ",tripletParen $ vector val," "]
showSymmetryOperation val@Reflection {}
  = concat [" m  ",prettyXYZ $ plane val]
showSymmetryOperation val@GlideABC {}
  = concat [" ",showABC $ abc val,"  ",prettyXYZ $ plane val]
showSymmetryOperation val@GlideDGN {}
  = concat [" ",showDGN $ dgn val," ",tripletParen $ glide val," ",prettyXYZ $ plane val]
showSymmetryOperation val@TwoFoldRotation {}
  = concat [" 2  ",prettyXYZ $ axis val]
showSymmetryOperation val@TwoFoldScrew {}
  = concat [" 2 ",tripletParen $ vector val," ",prettyXYZ $ axis val]
showSymmetryOperation val@NFoldRotation {}
  = concat [" ",showSymbol (nFold val),showSense (sense val)," ",prettyXYZ $ axis val]
showSymmetryOperation val@NFoldScrew {}
  = concat [" ",showSymbol (nFold val),showSense (sense val),tripletParen $ vector val," ",prettyXYZ $ axis val]
showSymmetryOperation val@Inversion {}
  = concat ["-1 ",triplet $ centre val]
showSymmetryOperation val@RotInversion {}
  = concat ["-",showSymbol (nFold val),showSense (sense val)," ",prettyXYZ $ axis val,"; ",triplet $ point val]


