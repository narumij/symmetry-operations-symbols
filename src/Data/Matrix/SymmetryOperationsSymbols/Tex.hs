module Data.Matrix.SymmetryOperationsSymbols.Tex where

import Control.Monad (join)

import Data.Ratio
import Data.Ratio.Slash
import Data.List
import Data.Matrix
import Data.Matrix.AsXYZ
import qualified Data.Matrix.AsXYZ.Tex as T
import Data.Matrix.SymmetryOperationsSymbols.SymopGeom

import Numeric

data TexPart = Str String | Space deriving Show

texRatio :: Integral a => Ratio a -> [T.Command]
texRatio n = [T.SizeS, T.Str . bar n . T.num $ n, T.SizeN]
  where
    bar n | n < 0 = T.bar
          | otherwise = id


-- |
-- >>> triplet (1%2,3%4,5%6)
-- "1/2,3/4,5/6"
triplet :: Integral a => (Ratio a,Ratio a,Ratio a) -> String
triplet (a,b,c) = intercalate "," . map (show . Slash) $ [a,b,c]

texTriplet :: Integral a => (Ratio a,Ratio a,Ratio a) -> [T.Command]
texTriplet (a,b,c) = intercalate [T.Str ","] . map texRatio $ [a,b,c]

-- |
-- >>> triplet (1%2,3%4,5%6)
-- "(1/2,3/4,5/6)"
tripletParen :: Integral a => (Ratio a,Ratio a,Ratio a) -> String
tripletParen = ("(" ++) . (++ ")") . triplet

texTripletParen :: Integral a => (Ratio a,Ratio a,Ratio a) -> [T.Command]
texTripletParen = ([T.SizeN,T.Str "("] ++) . (++ [T.SizeN,T.Str ")"]) . texTriplet


showSymbol :: NFold -> String
showSymbol ThreeFold = "3"
showSymbol FourFold  = "4"
showSymbol SixFold   = "6"

texSense :: Sense -> [T.Command]
texSense Positive = [T.Str "^+"]
texSense Negative = [T.Str "^-"]

texSense' = ([T.SizeS] ++) . (++[T.SizeN]) . texSense . sense

showABC :: ABC -> String
showABC A = "a"
showABC B = "b"
showABC C = "c"

showDGN :: DGN -> String
showDGN D = "d"
showDGN G = "g"
showDGN N = "n"

showVector :: Integral a => SymopGeom a -> String
showVector = tripletParen . vector

texVector :: Integral a => SymopGeom a -> [T.Command]
texVector = texTripletParen . vector

showPlane :: Integral a => SymopGeom a -> String
showPlane = prettyXYZ . fromLists . plane

texPlane :: Integral a => SymopGeom a -> [T.Command]
texPlane = T.commands "xyz" . take 3 . plane

showGlide :: Integral a => SymopGeom a -> String
showGlide = tripletParen . glide

texGlide :: Integral a => SymopGeom a -> [T.Command]
texGlide = texTripletParen . glide

showAxis :: Integral a => SymopGeom a -> String
showAxis = prettyXYZ . fromLists . axis

texAxis :: Integral a => SymopGeom a -> [T.Command]
texAxis = T.commands "xyz" . take 3 . axis

showPoint :: Integral a => SymopGeom a -> String
showPoint = triplet . point

texPoint :: Integral a => SymopGeom a -> [T.Command]
texPoint = texTriplet . point

texSpace :: [T.Command]
texSpace = [T.Str "\\ "]

texGeom :: Integral a => SymopGeom a -> [T.Command]
texGeom Identity = [T.SizeN, T.Str "1"]

texGeom val@Translation {}
  = [T.SizeN, T.Str "t"] ++ texVector val

texGeom val@Reflection {}
  = [T.SizeN, T.Str "m"] ++ texSpace ++ texPlane val

texGeom val@GlideABC {}
  = [T.SizeN, T.Str . showABC . abc $ val] ++ texSpace ++ texPlane val

texGeom val@GlideDGN {}
  = [T.SizeN, T.Str . showDGN . dgn $ val] ++ texGlide val ++ texSpace ++ texPlane val

texGeom val@TwoFoldRotation {}
  = [T.SizeN, T.Str "2"]  ++ texSpace ++ texAxis val

texGeom val@TwoFoldScrew {}
  = [T.SizeN, T.Str "2"] ++ texVector val ++ texSpace ++ texAxis val

texGeom val@NFoldRotation {}
  = [T.SizeN, T.Str . showSymbol . nFold $ val] ++ texSense' val ++ texSpace ++ texAxis val

texGeom val@NFoldScrew {}
  = [T.SizeN, T.Str . showSymbol . nFold $ val] ++ texSense' val ++ texVector val ++ texSpace ++ texAxis val

texGeom val@Inversion {}
  = [T.SizeN, T.Str $ T.label "overline" ++ T.curly "1"] ++ texSpace ++ (texTriplet . centre $ val)

texGeom val@RotInversion {}
  = [T.SizeN, T.Str $ T.label "overline" ++ T.curly (showSymbol (nFold val))] ++ texSense' val ++ texSpace ++ texAxis val  ++ texSpace ++ [T.Str "; "] ++ texSpace ++ texPoint val

toTex :: Integral a => SymopGeom a -> String
toTex = T.renderCommand T.Normalsize T.Small . T.reduceCommands . texGeom

test :: [SymopGeom Integer]
test = map read [
  "Identity",
  "Translation {vector = (0 % 1,1 % 2,1 % 2)}",
  "Reflection {plane = [[1 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,1 % 1,0 % 1]]}",
  "TwoFoldRotation {axis = [[0 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,1 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,0 % 1,0 % 1]]}",
  "TwoFoldScrew {axis = [[0 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,1 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,0 % 1,0 % 1]], vector = (0 % 1,1 % 2,0 % 1)}",
  "GlideABC {abc = C, plane = [[1 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,1 % 1,0 % 1]]}",
  "GlideDGN {dgn = N, plane = [[1 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,1 % 1,0 % 1]], glide = (1 % 2,0 % 1,1 % 2)}",
  "Inversion {centre = (1 % 4,1 % 4,1 % 4)}",
  "GlideDGN {dgn = D, plane = [[1 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,0 % 1,1 % 8],[0 % 1,0 % 1,1 % 1,0 % 1]], glide = (1 % 4,0 % 1,1 % 4)}",
  "NFoldRotation {nFold = FourFold, sense = Positive, axis = [[0 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,1 % 1,0 % 1]]}",
  "NFoldRotation {nFold = FourFold, sense = Negative, axis = [[0 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,1 % 1,0 % 1]]}",
  "NFoldScrew {nFold = FourFold, sense = Positive, axis = [[0 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,1 % 1,0 % 1]], vector = (0 % 1,0 % 1,1 % 4)}",
  "NFoldScrew {nFold = FourFold, sense = Negative, axis = [[0 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,1 % 1,0 % 1]], vector = (0 % 1,0 % 1,3 % 4)}",
  "Inversion {centre = (0 % 1,0 % 1,0 % 1)}",
  "RotInversion {nFold = FourFold, sense = Positive, axis = [[0 % 1,0 % 1,0 % 1,1 % 2],[0 % 1,0 % 1,0 % 1,1 % 4],[0 % 1,0 % 1,1 % 1,0 % 1]], point = (1 % 2,1 % 4,3 % 8)}",
  "RotInversion {nFold = FourFold, sense = Negative, axis = [[0 % 1,0 % 1,0 % 1,0 % 1],[0 % 1,0 % 1,0 % 1,1 % 4],[0 % 1,0 % 1,1 % 1,0 % 1]], point = (0 % 1,1 % 4,1 % 8)}"
  ]


