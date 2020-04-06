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

import Data.Matrix.SymmetryOperationsSymbols.Symbol

type ErrorMessage = String
type SymbolSenseVectorOrientation = (Symbol,String,String,String)

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
axisOf mat = fromJust $ fmap fromIntegral . axis <$> searchByRotationPart mat

senseOf :: (Integral a) => Matrix (Ratio a) -> String
senseOf mat = fromJust $ sense <$> searchByRotationPart mat

locationOf :: (Integral a) => Matrix (Ratio a) -> Matrix (Ratio a)
locationOf mat = fromJust $ location <$> searchByRotationPart mat

orientationOf :: (Integral a) => Matrix (Ratio a) -> [Ratio a]
orientationOf mat = fromJust $ orientation <$> searchByRotationPart mat

-- テーブルのレコードからほしい要素を取り出す関数たち
axis (a,s,b,c,d,e,f,g) = if null g then e else g
hex (a,s,b,c,d,e,f,g) = a
sense (_,_,_,s,_,_,_,_) = s
location (_,_,_,_,o,_,_,_) = rotPart . fromXYZ'' $ o
orientation (a,s,b,c,d,e,f,g) = fmap fromIntegral e

-- | 入力文字列が空だった場合に、4x4の0行列を返す
fromXYZ'' s = fromMaybe (zero 4 4) (fromXYZ' s)

-- | 入力の行列に該当するレコードを返却する
-- 行列の内容に対して一意なので、hexagonalを区別する必要がない
searchByRotationPart m = lookup (rotPart m) d
  where
    d = map (\i@(a,s,b,c,d,e,f,g)->(rotPart . fromXYZ'' $ f,i)) tbl

type MatrixLookupRecord = ((Symbol,Sense,Matrix (Ratio Integer)),TransformedCoordinate)

lookupMatrixM :: Monad m => String -> [MatrixLookupRecord] -> SymbolSenseVectorOrientation -> m TransformedCoordinate
lookupMatrixM reason dataTable (sy,se,_,el)
   = case lookupSSVO (primeSymbol sy, se, el) dataTable of
      Nothing -> fail reason
      Just c  -> return c

lookupSSVO :: (Symbol,String,String) -> [MatrixLookupRecord] -> Maybe TransformedCoordinate
lookupSSVO (sym, sen, axis) d = lookup (sym, sen, rotPart . fromXYZ'' $ axis) d

properMatrixW :: Monad m => SymbolSenseVectorOrientation -> m TransformedCoordinate
properMatrixW = lookupMatrixM "matrix W not found (proper)." (fromTbl properTbl)

hexagonalMatrixW :: Monad m => SymbolSenseVectorOrientation -> m TransformedCoordinate
hexagonalMatrixW = lookupMatrixM "matrix W not found (hexagonal)." (fromTbl hexagonalTbl)

-- lookup' :: [Tbl] -> PointGroupSymmetryOperations
-- lookupSSVO tbl= lookupSSVO' (tblSSVO tbl)

fromTbl :: [Tbl] -> [MatrixLookupRecord]
fromTbl = map tblToMLR

tblToMLR (a,s,b,c,d,e,f,g) = ((s,c,rotPart . fromXYZ'' $ d),f)

properTbl :: [Tbl]
properTbl = filter (not . hex) tbl

hexagonalTbl :: [Tbl]
hexagonalTbl = filter hex tbl ++ filter (not . hex) tbl


primeSymbol :: Symbol -> Symbol
primeSymbol T = Id
primeSymbol A = M
primeSymbol B = M
primeSymbol C = M
primeSymbol D = M
primeSymbol N = M
primeSymbol G = M
primeSymbol otherSymbol = otherSymbol

type IsHex = Bool -- hex flag
type SymbolLabel = String
type Sense = String
type SymmetryElement = String
type Orientation = [Integer] -- orientation or location
type TransformedCoordinate = String
type AxisOrNormal = [Integer]

type Tbl = (IsHex,Symbol,SymbolLabel,Sense,SymmetryElement,Orientation,TransformedCoordinate,AxisOrNormal)

tbl :: [Tbl]
tbl = [
-- Table 11.2.2.1. Matrices for point-group symmetry operations and orientation
-- of corresponding symmetry elements, referred to a cubic, tetragonal, orthorhombic,
-- monoclinic, triclinic or rhombohedral coordinate system
  (False,  Id,  "1",  "",        "",         [],     "x,y,z", []),
  (False,  R2,  "2",  "",   "0,0,z", [ 0, 0, 1],   "-x,-y,z", []),
  (False,  R2,  "2",  "",   "0,y,0", [ 0, 1, 0],   "-x,y,-z", []),
  (False,  R2,  "2",  "",   "x,0,0", [ 1, 0, 0],   "x,-y,-z", []),
  (False,  R3,  "3", "+",   "x,x,x", [ 1, 1, 1],     "z,x,y", []),
  (False,  R3,  "3", "+", "x,-x,-x", [ 1,-1,-1],   "-z,-x,y", []),
  (False,  R3,  "3", "+", "-x,x,-x", [-1, 1,-1],   "z,-x,-y", []),
  (False,  R3,  "3", "+", "-x,-x,x", [-1,-1, 1],   "-z,x,-y", []),
  (False,  R3,  "3", "-",   "x,x,x", [ 1, 1, 1],     "y,z,x", []),
  (False,  R3,  "3", "-", "x,-x,-x", [ 1,-1,-1],   "-y,z,-x", []),
  (False,  R3,  "3", "-", "-x,x,-x", [-1, 1,-1],   "-y,-z,x", []),
  (False,  R3,  "3", "-", "-x,-x,x", [-1,-1, 1],   "y,-z,-x", []),
  (False,  R2,  "2",  "",   "x,x,0", [ 1, 1, 0],    "y,x,-z", []),
  (False,  R2,  "2",  "",   "x,0,x", [ 1, 0, 1],    "z,-y,x", []),
  (False,  R2,  "2",  "",   "0,y,y", [ 0, 1, 1],    "-x,z,y", []),
  (False,  R2,  "2",  "",  "x,-x,0", [ 1,-1, 0],  "-y,-x,-z", []),
  (False,  R2,  "2",  "",  "-x,0,x", [-1, 0, 1],  "-z,-y,-x", []),
  (False,  R2,  "2",  "",  "0,y,-y", [ 0, 1,-1],  "-x,-z,-y", []),
  (False,  R4,  "4", "+",   "0,0,z", [ 0, 0, 1],    "-y,x,z", []),
  (False,  R4,  "4", "+",   "0,y,0", [ 0, 1, 0],    "z,y,-x", []),
  (False,  R4,  "4", "+",   "x,0,0", [ 1, 0, 0],    "x,-z,y", []),
  (False,  R4,  "4", "-",   "0,0,z", [ 0, 0, 1],    "y,-x,z", []),
  (False,  R4,  "4", "-",   "0,y,0", [ 0, 1, 0],    "-z,y,x", []),
  (False,  R4,  "4", "-",   "x,0,0", [ 1, 0, 0],    "x,z,-y", []),
----
  (False, Inv, "-1",  "",   "0,0,0",         [],  "-x,-y,-z", []),
  (False,   M,  "m",  "",   "x,y,0", [ 0, 0, 1],    "x,y,-z", []),
  (False,   M,  "m",  "",   "x,0,z", [ 0, 1, 0],    "x,-y,z", []),
  (False,   M,  "m",  "",   "0,y,z", [ 1, 0, 0],    "-x,y,z", []),
  (False, RI3, "-3", "+",   "x,x,x", [ 1, 1, 1],  "-z,-x,-y", []),
  (False, RI3, "-3", "+", "x,-x,-x", [ 1,-1,-1],    "z,x,-y", []),
  (False, RI3, "-3", "+", "-x,x,-x", [-1, 1,-1],    "-z,x,y", []),
  (False, RI3, "-3", "+", "-x,-x,x", [-1,-1, 1],    "z,-x,y", []),
  (False, RI3, "-3", "-",   "x,x,x", [ 1, 1, 1],  "-y,-z,-x", []),
  (False, RI3, "-3", "-", "x,-x,-x", [ 1,-1,-1],    "y,-z,x", []),
  (False, RI3, "-3", "-", "-x,x,-x", [-1, 1,-1],    "y,z,-x", []),
  (False, RI3, "-3", "-", "-x,-x,x", [-1,-1, 1],    "-y,z,x", []),
  (False,   M,  "m",  "",  "x,-x,z", [ 1, 1, 0],   "-y,-x,z", []),
  (False,   M,  "m",  "",  "-x,y,x", [ 1, 0, 1],   "-z,y,-x", []),
  (False,   M,  "m",  "",  "x,y,-y", [ 0, 1, 1],   "x,-z,-y", []),
  (False,   M,  "m",  "",   "x,x,z", [ 1,-1, 0],     "y,x,z", []),
  (False,   M,  "m",  "",   "x,y,x", [-1, 0, 1],     "z,y,x", []),
  (False,   M,  "m",  "",   "x,y,y", [ 0, 1,-1],     "x,z,y", []),
  (False, RI4, "-4", "+",   "0,0,z", [ 0, 0, 1],   "y,-x,-z", []),
  (False, RI4, "-4", "+",   "0,y,0", [ 0, 1, 0],   "-z,-y,x", []),
  (False, RI4, "-4", "+",   "x,0,0", [ 1, 0, 0],   "-x,z,-y", []),
  (False, RI4, "-4", "-",   "0,0,z", [ 0, 0, 1],   "-y,x,-z", []),
  (False, RI4, "-4", "-",   "0,y,0", [ 0, 1, 0],   "z,-y,-x", []),
  (False, RI4, "-4", "-",   "x,0,0", [ 1, 0, 0],   "-x,-z,y", []),
-- Table 11.2.2.2. Matrices for point-group symmetry operations and orientation
-- of corresponding symmetry elements, referred to a hexagonal coordinate system
  ( True,  Id,  "1",  "",        "",         [],     "x,y,z", []),
  ( True,  R3,  "3", "+",   "0,0,z", [ 0, 0, 1],  "-y,x-y,z", []),
  ( True,  R3,  "3", "-",   "0,0,z", [ 0, 0, 1],  "y-x,-x,z", []),
  ( True,  R2,  "2",  "",   "0,0,z", [ 0, 0, 1],   "-x,-y,z", []),
  ( True,  R6,  "6", "+",   "0,0,z", [ 0, 0, 1],   "x-y,x,z", []),
  ( True,  R6,  "6", "-",   "0,0,z", [ 0, 0, 1],   "y,y-x,z", []),
  ( True,  R2,  "2",  "",   "x,x,0", [ 1, 1, 0],    "y,x,-z", []),
  ( True,  R2,  "2",  "",   "x,0,0", [ 1, 0, 0], "x-y,-y,-z", []),
  ( True,  R2,  "2",  "",   "0,y,0", [ 0, 1, 0], "-x,y-x,-z", []),
  ( True,  R2,  "2",  "",  "x,-x,0", [ 1,-1, 0],  "-y,-x,-z", []),
  -- 
  ( True,  R2,  "2",  "",  "x,2x,0", [ 1, 2, 0],  "y-x,y,-z", []),
  ( True,  R2,  "2",  "",  "2x,x,0", [ 2, 1, 0],  "x,x-y,-z", []),
  -- 
  ( True, Inv, "-1",  "",   "0,0,0",         [],  "-x,-y,-z", []),
  ( True, RI3, "-3", "+",   "0,0,z", [ 0, 0, 1],  "y,y-x,-z", []),
  ( True, RI3, "-3", "-",   "0,0,z", [ 0, 0, 1],  "x-y,x,-z", []),
  ( True,   M,  "m",  "",   "x,y,0", [ 0, 0, 1],    "x,y,-z", []),
  ( True, RI6, "-6", "+",   "0,0,z", [ 0, 0, 1], "y-x,-x,-z", []),
  ( True, RI6, "-6", "-",   "0,0,z", [ 0, 0, 1], "-y,x-y,-z", []),
  ( True,   M,  "m",  "",  "x,-x,z", [ 1, 1, 0],   "-y,-x,z", []),
-- 以下二つが、Orientationを利用して解の補正をすることができなかったため、代替値を用意している
-- 行列を解いた場合の解平面とorientationが一致していない可能性（2017の試行錯誤)
-- そもそも勘違いの可能性もまだあるので、のちのち再確認する。
-- hackageに登録するに至らない理由の一つ
-- 正規化された値として正しいが、正規化の結果、復元に必要な情報が欠落してしまった可能性(2020リファクタリング時の見解)
-- どうしてこうなっているのか、やっぱりわからない。
  ( True,   M,  "m",  "",  "x,2x,z", [ 1, 0, 0],   "y-x,y,z", [ 2,-1, 0]),
  ( True,   M,  "m",  "",  "2x,x,z", [ 0, 1, 0],   "x,x-y,z", [-1, 2, 0]),

  ( True,   M,  "m",  "",   "x,x,z", [ 1,-1, 0],     "y,x,z", []),
  ( True,   M,  "m",  "",   "x,0,z", [ 1, 2, 0],  "x-y,-y,z", []),
  ( True,   M,  "m",  "",   "0,y,z", [ 2, 1, 0],  "-x,y-x,z", [])
  -- Notice
  -- Hexagonal用のテーブルが、HexagonalのITで出現する対称操作全てをカバーしているわけではないことに注意
  -- hexagonalでのlookup時には、hexagonal部分が優先となるよう、順番をいれかえている
  -- それ以外のケースでは除外している
  ]
