{-|
Module      : Common
Description : utilities
Copyright   : (c) Jun Narumi, 2018
License     : BSD-3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

[Reference]

W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets

listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

-}
module Data.Matrix.SymmetryOperationsSymbols.Common (
  ErrorMessage,
  SymbolSenseVectorOrientation,
  -- maybeToEither,
  rotPart,
  transPart,
  iw,
  triplet,
  tripletParen,
  adjustAnswerOnAxis,
  axisOf,
  senseOf,
  locationOf,
  orientationOf,
  properMatrixW,
  hexagonalMatrixW,
  fromXYZ'',
  ) where

import Data.List
import Data.Ratio
import Data.Matrix
import Data.Maybe
import Data.Matrix.AsXYZ
import Data.Ratio.Slash

type ErrorMessage = String
type SymbolSenseVectorOrientation = (String,String,String,String)

-- | borrowed from Base ?.?.?
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a  Nothing = Left a

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

-- | calculate (I-W)
iw :: Num c => Matrix c -> Matrix c
iw = elementwise (-) (identity 3) . rotPart

-- | 3x3 rotation part of matrix
rotPart :: Matrix a -> Matrix a
rotPart = submatrix 1 3 1 3

-- | 3x1 translation part of matrix
transPart :: Matrix a -> Matrix a
transPart = submatrix 1 3 4 4

-- | 解を解直線上で補正
adjustAnswerOnAxis :: (Eq b, Fractional b, Integral a) =>  Matrix (Ratio a) -> [b] -> Maybe [b]
adjustAnswerOnAxis mat ans = do
  let basis = axisOf mat
  adjustRate <- rate ans basis
  -- 算出した補正量と解の積を返す
  return $ zipWith (+) ans (fmap (*adjustRate) basis)

-- 計算対象となる要素の補正値
-- 解の直線を算出することができなかったので、データを参照することで代替している
rate :: (Eq a, Fractional a) => [a] -> [a] -> Maybe a
rate ans [] = Just 1
rate ans basis
  = listToMaybe
    . map snd . filter fst . zip select
    $ zipWith rating ans basis
  where
    rating ans basis = if basis == 0 then 0 else -ans / basis
    select = case basis of
        [ 0, 0, 1] -> [False,False, True] -- OK
        [ 0, 1, 0] -> [False, True,False] -- OK
        [ 1, 0, 0] -> [ True,False,False] -- OK
        [ 1, 1, 1] -> [False,False, True] -- ??
        [ 1,-1,-1] -> [False,False, True] -- OK
        [-1,-1, 1] -> [False,False, True] -- OK
        [-1, 1,-1] -> [False,False, True] -- OK
        [ 1, 1, 0] -> [ True,False,False] -- OK
        [ 1, 0, 1] -> [False,False, True] -- OK
        [ 0, 1, 1] -> [False,False, True] -- OK
        [ 1,-1, 0] -> [ True,False,False] -- OK
        [-1, 0, 1] -> [False,False, True] -- OK
        [ 0, 1,-1] -> [False,False, True] -- OK
        [ 1, 2, 0] -> [False, True,False] -- ??
        [ 2, 1, 0] -> [ True,False,False] -- ??
        _ -> error ""

----------------------

axisOf :: (Integral a,Num b) => Matrix (Ratio a) -> [b]
axisOf mat = fromJust $ fmap fromIntegral . axis <$> searchMapData mat

senseOf :: (Integral a) => Matrix (Ratio a) -> String
senseOf mat = fromJust $ sense <$> searchMapData mat

locationOf :: (Integral a) => Matrix (Ratio a) -> Matrix (Ratio a)
locationOf mat = fromJust $ location <$> searchMapData mat

orientationOf :: (Integral a) => Matrix (Ratio a) -> [Ratio a]
orientationOf mat = fromJust $ orientation <$> searchMapData mat

axis (a,b,c,d,e,f,g) = g
hex (a,b,c,d,e,f,g) = a
sense (_,_,s,_,_,_,_) = s
location (_,_,_,o,_,_,_) = rotPart . fromXYZ'' $ o
orientation (a,b,c,d,e,f,g) = fmap fromIntegral e

-- | 入力文字列が空だった場合に、4x4の0行列を返す
fromXYZ'' s = fromMaybe (zero 4 4) (fromXYZ' s)

searchMapData m = lookup (rotPart m) d
  where
    d = map (\i@(a,b,c,d,e,f,g)->(rotPart . fromXYZ'' $ f,i)) tbl

lookupM' :: Monad m => [Tbl] -> String -> SymbolSenseVectorOrientation -> m TransformedCoordinate
lookupM' dataTable reason (sy,se,_,el)
   = case (lookup' dataTable sy se el) of
      Nothing -> fail reason
      Just c  -> return c

properMatrixW :: Monad m => SymbolSenseVectorOrientation -> m TransformedCoordinate
properMatrixW = lookupM' dataTable "matrix W not found (proper)."
    where
      dataTable = filter (not . hex) tbl

hexagonalMatrixW :: Monad m => SymbolSenseVectorOrientation -> m TransformedCoordinate
hexagonalMatrixW = lookupM' dataTable "matrix W not found (hexagonal)."
  where
    dataTable = filter hex tbl ++ filter (not . hex) tbl

-- lookup' :: [Tbl] -> PointGroupSymmetryOperations
lookup' tbl sym sen axis = lookup (a',sen,rotPart . fromXYZ'' $ axis) d
  where
    a' | sym `elem` ["a","b","c","d","n","g"] = "m"
       | sym == "t" = "1"
       | otherwise = sym
    d = map f tbl
    f (a,b,c,d,e,f,g) = ((b,c,rotPart . fromXYZ'' $ d),f)

type IsHex = Bool -- hex flag
type Symbol = String
type Sense = String
type SymmetryElement = String
type Orientation = [Integer] -- orientation or location
type TransformedCoordinate = String
type AxisOrNormal = [Integer]

type Tbl = (IsHex,Symbol,Sense,SymmetryElement,Orientation,TransformedCoordinate,AxisOrNormal)

tbl :: [Tbl]
tbl = [
-- Table 11.2.2.1. Matrices for point-group symmetry operations and orientation
-- of corresponding symmetry elements, referred to a cubic, tetragonal, orthorhombic,
-- monoclinic, triclinic or rhombohedral coordinate system
  (False,  "1",  "",        "",         [],     "x,y,z",         []),
  (False,  "2",  "",   "0,0,z", [ 0, 0, 1],   "-x,-y,z", [ 0, 0, 1]),
  (False,  "2",  "",   "0,y,0", [ 0, 1, 0],   "-x,y,-z", [ 0, 1, 0]),
  (False,  "2",  "",   "x,0,0", [ 1, 0, 0],   "x,-y,-z", [ 1, 0, 0]),
  (False,  "3", "+",   "x,x,x", [ 1, 1, 1],     "z,x,y", [ 1, 1, 1]),
  (False,  "3", "+", "x,-x,-x", [ 1,-1,-1],   "-z,-x,y", [ 1,-1,-1]),
  (False,  "3", "+", "-x,x,-x", [-1, 1,-1],   "z,-x,-y", [-1, 1,-1]),
  (False,  "3", "+", "-x,-x,x", [-1,-1, 1],   "-z,x,-y", [-1,-1, 1]),
  (False,  "3", "-",   "x,x,x", [ 1, 1, 1],     "y,z,x", [ 1, 1, 1]),
  (False,  "3", "-", "x,-x,-x", [ 1,-1,-1],   "-y,z,-x", [ 1,-1,-1]),
  (False,  "3", "-", "-x,x,-x", [-1, 1,-1],   "-y,-z,x", [-1, 1,-1]),
  (False,  "3", "-", "-x,-x,x", [-1,-1, 1],   "y,-z,-x", [-1,-1, 1]),
  (False,  "2",  "",   "x,x,0", [ 1, 1, 0],    "y,x,-z", [ 1, 1, 0]),
  (False,  "2",  "",   "x,0,x", [ 1, 0, 1],    "z,-y,x", [ 1, 0, 1]),
  (False,  "2",  "",   "0,y,y", [ 0, 1, 1],    "-x,z,y", [ 0, 1, 1]),
  (False,  "2",  "",  "x,-x,0", [ 1,-1, 0],  "-y,-x,-z", [ 1,-1, 0]),
  (False,  "2",  "",  "-x,0,x", [-1, 0, 1],  "-z,-y,-x", [-1, 0, 1]),
  (False,  "2",  "",  "0,y,-y", [ 0, 1,-1],  "-x,-z,-y", [ 0, 1,-1]),
  (False,  "4", "+",   "0,0,z", [ 0, 0, 1],    "-y,x,z", [ 0, 0, 1]),
  (False,  "4", "+",   "0,y,0", [ 0, 1, 0],    "z,y,-x", [ 0, 1, 0]),
  (False,  "4", "+",   "x,0,0", [ 1, 0, 0],    "x,-z,y", [ 1, 0, 0]),
  (False,  "4", "-",   "0,0,z", [ 0, 0, 1],    "y,-x,z", [ 0, 0, 1]),
  (False,  "4", "-",   "0,y,0", [ 0, 1, 0],    "-z,y,x", [ 0, 1, 0]),
  (False,  "4", "-",   "x,0,0", [ 1, 0, 0],    "x,z,-y", [ 1, 0, 0]),
----
  (False, "-1",  "",   "0,0,0",         [],  "-x,-y,-z",         []),
  (False,  "m",  "",   "x,y,0", [ 0, 0, 1],    "x,y,-z", [ 0, 0, 1]),
  (False,  "m",  "",   "x,0,z", [ 0, 1, 0],    "x,-y,z", [ 0, 1, 0]),
  (False,  "m",  "",   "0,y,z", [ 1, 0, 0],    "-x,y,z", [ 1, 0, 0]),
  (False, "-3", "+",   "x,x,x", [ 1, 1, 1],  "-z,-x,-y", [ 1, 1, 1]),
  (False, "-3", "+", "x,-x,-x", [ 1,-1,-1],    "z,x,-y", [ 1,-1,-1]),
  (False, "-3", "+", "-x,x,-x", [-1, 1,-1],    "-z,x,y", [-1, 1,-1]),
  (False, "-3", "+", "-x,-x,x", [-1,-1, 1],    "z,-x,y", [-1,-1, 1]),
  (False, "-3", "-",   "x,x,x", [ 1, 1, 1],  "-y,-z,-x", [ 1, 1, 1]),
  (False, "-3", "-", "x,-x,-x", [ 1,-1,-1],    "y,-z,x", [ 1,-1,-1]),
  (False, "-3", "-", "-x,x,-x", [-1, 1,-1],    "y,z,-x", [-1, 1,-1]),
  (False, "-3", "-", "-x,-x,x", [-1,-1, 1],    "-y,z,x", [-1,-1, 1]),
  (False,  "m",  "",  "x,-x,z", [ 1, 1, 0],   "-y,-x,z", [ 1, 1, 0]),
  (False,  "m",  "",  "-x,y,x", [ 1, 0, 1],   "-z,y,-x", [ 1, 0, 1]),
  (False,  "m",  "",  "x,y,-y", [ 0, 1, 1],   "x,-z,-y", [ 0, 1, 1]),
  (False,  "m",  "",   "x,x,z", [ 1,-1, 0],     "y,x,z", [ 1,-1, 0]),
  (False,  "m",  "",   "x,y,x", [-1, 0, 1],     "z,y,x", [-1, 0, 1]),
  (False,  "m",  "",   "x,y,y", [ 0, 1,-1],     "x,z,y", [ 0, 1,-1]),
  (False, "-4", "+",   "0,0,z", [ 0, 0, 1],   "y,-x,-z", [ 0, 0, 1]),
  (False, "-4", "+",   "0,y,0", [ 0, 1, 0],   "-z,-y,x", [ 0, 1, 0]),
  (False, "-4", "+",   "x,0,0", [ 1, 0, 0],   "-x,z,-y", [ 1, 0, 0]),
  (False, "-4", "-",   "0,0,z", [ 0, 0, 1],   "-y,x,-z", [ 0, 0, 1]),
  (False, "-4", "-",   "0,y,0", [ 0, 1, 0],   "z,-y,-x", [ 0, 1, 0]),
  (False, "-4", "-",   "x,0,0", [ 1, 0, 0],   "-x,-z,y", [ 1, 0, 0]),
-- Table 11.2.2.2. Matrices for point-group symmetry operations and orientation
-- of corresponding symmetry elements, referred to a hexagonal coordinate system
  ( True,  "1",  "",        "",         [],     "x,y,z",         []),
  ( True,  "3", "+",   "0,0,z", [ 0, 0, 1],  "-y,x-y,z", [ 0, 0, 1]),
  ( True,  "3", "-",   "0,0,z", [ 0, 0, 1],  "y-x,-x,z", [ 0, 0, 1]),
  ( True,  "2",  "",   "0,0,z", [ 0, 0, 1],   "-x,-y,z", [ 0, 0, 1]),
  ( True,  "6", "+",   "0,0,z", [ 0, 0, 1],   "x-y,x,z", [ 0, 0, 1]),
  ( True,  "6", "-",   "0,0,z", [ 0, 0, 1],   "y,y-x,z", [ 0, 0, 1]),
  ( True,  "2",  "",   "x,x,0", [ 1, 1, 0],    "y,x,-z", [ 1, 1, 0]),
  ( True,  "2",  "",   "x,0,0", [ 1, 0, 0], "x-y,-y,-z", [ 1, 0, 0]),
  ( True,  "2",  "",   "0,y,0", [ 0, 1, 0], "-x,y-x,-z", [ 0, 1, 0]),
  ( True,  "2",  "",  "x,-x,0", [ 1,-1, 0],  "-y,-x,-z", [ 1,-1, 0]),
  ( True,  "2",  "",  "x,2x,0", [ 1, 2, 0],  "y-x,y,-z", [ 1, 2, 0]),
  ( True,  "2",  "",  "2x,x,0", [ 2, 1, 0],  "x,x-y,-z", [ 2, 1, 0]),
  ( True, "-1",  "",   "0,0,0",         [],  "-x,-y,-z",         []),
  ( True, "-3", "+",   "0,0,z", [ 0, 0, 1],  "y,y-x,-z", [ 0, 0, 1]),
  ( True, "-3", "-",   "0,0,z", [ 0, 0, 1],  "x-y,x,-z", [ 0, 0, 1]),
  ( True,  "m",  "",   "x,y,0", [ 0, 0, 1],    "x,y,-z", [ 0, 0, 1]),
  ( True, "-6", "+",   "0,0,z", [ 0, 0, 1], "y-x,-x,-z", [ 0, 0, 1]),
  ( True, "-6", "-",   "0,0,z", [ 0, 0, 1], "-y,x-y,-z", [ 0, 0, 1]),
  ( True,  "m",  "",  "x,-x,z", [ 1, 1, 0],   "-y,-x,z", [ 1, 1, 0]),
-- 以下二つが、Orientationを利用して解の補正をすることができなかったため、代替値を用意している
-- 行列を解いた場合の解平面とorientationが一致していない可能性（2017の試行錯誤)
-- そもそも勘違いの可能性もまだあるので、のちのち再確認する。
  ( True,  "m",  "",  "x,2x,z", [ 1, 0, 0],   "y-x,y,z", [ 2,-1, 0]),
  ( True,  "m",  "",  "2x,x,z", [ 0, 1, 0],   "x,x-y,z", [-1, 2, 0]),

  ( True,  "m",  "",   "x,x,z", [ 1,-1, 0],     "y,x,z", [ 1,-1, 0]),
  ( True,  "m",  "",   "x,0,z", [ 1, 2, 0],  "x-y,-y,z", [ 1, 2, 0]),
  ( True,  "m",  "",   "0,y,z", [ 2, 1, 0],  "-x,y-x,z", [ 2, 1, 0])
  -- Notice
  -- Hexagonal用のテーブルが、HexagonalのITで出現する対称操作全てをカヴァーしているわけではないことに注意
  -- hexagonalでのlookup時には、hexagonal部分が優先となるよう、順番をいれかえている
  -- それ以外のケースでは除外している
  ]
