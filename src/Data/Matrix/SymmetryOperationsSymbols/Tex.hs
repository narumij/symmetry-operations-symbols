module Data.Matrix.SymmetryOperationsSymbols.Tex where

import Data.Ratio
import Data.Ratio.Slash
import Data.List
import Data.Matrix.AsXYZ
import Data.Matrix.SymmetryOperationsSymbols.SymmetryOperation

data TexPart = Str String deriving Show

{-

label :: String -> String
label l = "\\" ++ l

curly :: String -> String
curly a = "{" ++ a ++ "}"

bar :: String -> String
bar a = label "overline" ++ curly a

-- |
-- >>> triplet (1%2,3%4,5%6)
-- "1/2,3/4,5/6"
triplet :: Integral a => (Ratio a,Ratio a,Ratio a) -> String
triplet (a,b,c) = intercalate "," . map (show . Slash) $ [a,b,c]

-- |
-- >>> triplet (1%2,3%4,5%6)
-- "(1/2,3/4,5/6)"
tripletParen :: Integral a => (Ratio a,Ratio a,Ratio a) -> String
tripletParen = ("(" ++) . (++ ")") . triplet

showSymbol :: NFold -> String
showSymbol ThreeFold = "3"
showSymbol FourFold  = "4"
showSymbol SixFold   = "6"

texSense :: Sense -> String
texSense Positive = "^+"
texSense Negative = "^-"

showABC :: ABC -> String
showABC A = "a"
showABC B = "b"
showABC C = "c"

showDGN :: DGN -> String
showDGN D = "d"
showDGN G = "g"
showDGN N = "n"


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

-}