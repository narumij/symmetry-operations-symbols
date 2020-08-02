module Data.Matrix.SymmetryOperationsSymbols.Tex where

import Data.Ratio (Ratio)
import Data.List (intercalate)
import qualified Data.Matrix.AsXYZ.Tex as T
import Data.Matrix.SymmetryOperationsSymbols.SymopGeom
  (SymopGeom(Identity,Translation,Reflection,GlideABC,GlideDGN,
      TwoFoldRotation,TwoFoldScrew,NFoldRotation,NFoldScrew,
      Inversion,RotInversion),Sense(..))
import qualified Data.Matrix.SymmetryOperationsSymbols.SymopGeom as S

make n a = concatMap ($ a) $ [begin] ++ n
  where
    begin _ = [T.SizeN]

sep a = [T.Str ";"]

sp a = [T.Str "\\ "]

s str _ = [T.Str str]

abc = flip s () . S.showABC . S.abc

dgn = flip s () . S.showDGN . S.dgn

n = flip s () . S.showSymbol . S.nFold

bar' n a = (:[]) . T.Str . T.label . ("overline" ++) . T.curly $ n

bar n = bar' n

barN = flip bar () . S.showSymbol . S.nFold

texRatio :: Integral a => Ratio a -> [T.Command]
texRatio n = [T.SizeS, T.Str . bar n . T.num $ n, T.SizeN]
  where
    bar n | n < 0 = T.bar
          | otherwise = id

texTriplet :: Integral a => (Ratio a,Ratio a,Ratio a) -> [T.Command]
texTriplet (a,b,c) = intercalate [T.Str ","] . map texRatio $ [a,b,c]

texTripletParen :: Integral a => (Ratio a,Ratio a,Ratio a) -> [T.Command]
texTripletParen = ([T.SizeN,T.Str "("] ++) . (++ [T.SizeN,T.Str ")"]) . texTriplet

sense :: SymopGeom a -> [T.Command]
sense = ([T.SizeS] ++) . (++[T.SizeN]) . sense' . S.sense
  where
    sense' Positive = [T.Str "^+"]
    sense' Negative = [T.Str "^-"]

vector :: Integral a => SymopGeom a -> [T.Command]
vector = texTripletParen . S.vector

plane :: Integral a => SymopGeom a -> [T.Command]
plane = T.commands "xyz" . take 3 . S.plane

glide :: Integral a => SymopGeom a -> [T.Command]
glide = texTripletParen . S.glide

axis :: Integral a => SymopGeom a -> [T.Command]
axis = T.commands "xyz" . take 3 . S.axis

centre :: Integral a => SymopGeom a -> [T.Command]
centre = texTriplet . S.centre

point :: Integral a => SymopGeom a -> [T.Command]
point = texTriplet . S.point


texGeom :: Integral a => SymopGeom a -> [T.Command]
texGeom Identity
  = make [s "1"] ()

texGeom t@Translation {}
  = make [s "t",vector] t

texGeom mm@Reflection {}
  = make [s "m", sp, plane] mm

texGeom mm@GlideABC {}
  = make [abc, sp, plane] mm

texGeom mm@GlideDGN {}
  = make [dgn, glide, sp, plane] mm

texGeom rr@TwoFoldRotation {}
  = make [s "2", sp, axis] rr

texGeom ss@TwoFoldScrew {}
  = make [s "2", vector, sp, axis] ss

texGeom rr@NFoldRotation {}
  = make [n, sense, sp, axis] rr

texGeom ss@NFoldScrew {}
  = make [n, sense, vector, sp, axis] ss

texGeom ii@Inversion {}
  = make [bar "1", sp, centre] ii

texGeom ii@RotInversion {}
  = make [barN, sense, sp, axis, sp, sep, sp, point] ii

toTex' :: Integral a => T.Relative -> T.Relative -> SymopGeom a -> String
toTex' sizeN sizeS = T.renderCommand sizeN sizeS . T.reduceCommands . texGeom

toTex :: Integral a => SymopGeom a -> String
toTex = toTex' T.Normalsize T.Small



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


