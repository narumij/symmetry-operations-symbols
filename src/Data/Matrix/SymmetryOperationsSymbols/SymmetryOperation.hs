module Data.Matrix.SymmetryOperationsSymbols.SymmetryOperation (
    SymmetryOperation(..),
    NFold(..),
    Sense(..),
    ABC(..),
    DGN(..),
    showSymmetryOperation,
    )
    where

import Data.Ratio
import Data.Matrix
import Data.Matrix.AsXYZ
import Data.Matrix.SymmetryOperationsSymbols.Common

data ABC = A | B | C deriving (Show,Eq)
data DGN = D | G | N deriving (Show,Eq)
data NFold = ThreeFold | FourFold | SixFold deriving (Show,Eq)
data Sense = Positive | Negative deriving (Show,Eq)

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
  deriving (Show,Eq)

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


data TexPart = Str String deriving Show

label :: String -> String
label l = "\\" ++ l

curly :: String -> String
curly a = "{" ++ a ++ "}"

bar :: String -> String
bar a = label "overline" ++ curly a

texSymmetryOperation :: Integral a => SymmetryOperation a -> [TexPart]

texSymmetryOperation Identity = [Str "1"]

texSymmetryOperation val@Translation {}
  = [Str $ concat ["t", tripletParen $ vector val," "]]

texSymmetryOperation val@Reflection {}
  = [Str $ concat ["m", prettyXYZ $ plane val]]

texSymmetryOperation val@GlideABC {}
  = [Str $ concat [showABC $ abc val,"  ",prettyXYZ $ plane val]]

texSymmetryOperation val@GlideDGN {}
  = [Str $ concat [showDGN $ dgn val," ",tripletParen $ glide val," ",prettyXYZ $ plane val]]

texSymmetryOperation val@TwoFoldRotation {}
  = [Str $ concat ["2",prettyXYZ $ axis val]]

texSymmetryOperation val@TwoFoldScrew {}
  = [Str $ concat ["2",tripletParen $ vector val," ",prettyXYZ $ axis val]]

texSymmetryOperation val@NFoldRotation {}
  = [Str $ concat [showSymbol (nFold val),showSense (sense val)," ",prettyXYZ $ axis val]]

texSymmetryOperation val@NFoldScrew {}
  = [Str $ concat [showSymbol (nFold val),showSense (sense val),tripletParen $ vector val," ",prettyXYZ $ axis val]]

texSymmetryOperation val@Inversion {}
  = [Str $ concat [label "overline" ++ curly "1", triplet $ centre val]]

texSymmetryOperation val@RotInversion {}
  = [Str $ concat [label "overline" ++ curly (showSymbol (nFold val)),showSense (sense val)," ",prettyXYZ $ axis val,"; ",triplet $ point val]]

