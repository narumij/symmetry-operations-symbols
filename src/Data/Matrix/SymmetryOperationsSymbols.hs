module Data.Matrix.SymmetryOperationsSymbols (
  fromMatrix,
  toMatrix,
  toMatrixHex,
  ) where

import Data.List
import Data.Ratio
import Data.Ratio.Slash
import Data.Matrix
import Data.Fixed
import Data.Maybe

import Data.Matrix.SymmetryOperationsSymbols.Common
import Data.Matrix.SymmetryOperationsSymbols.Parse

import Data.Matrix.SymmetryOperationsSymbols.UnitMatrixCase
import Data.Matrix.SymmetryOperationsSymbols.GlideOrReflectionCase
import Data.Matrix.SymmetryOperationsSymbols.RotationCase
import Data.Matrix.SymmetryOperationsSymbols.RotInversionCase

-- for doctest
import Data.Matrix.AsXYZ

-- | 与えられた対称要素の行列から、対称操作の記号等を導出します。
-- input: 3x4又は4x4のMatrix Rational
--
-- >>> fromMatrix . fromXYZ $ " 1 "
-- "x,y,z"
--
fromMatrix :: Matrix Rational -> String
fromMatrix m = case fromMatrix' m of
  Left s -> error s
  Right s -> s

fromMatrix' :: Matrix Rational -> Either String String
fromMatrix' m
-- (i)
  | rotPart m == identity 3             = unitMatrixCase m
-- (ii) (a)
  | correpondToRotInversion tr det      = rotInversionCase m
-- (ii) (b)
  | correpondToNFoldRotation tr det     = nFoldRotationCase m
-- (ii) (c)
  | correpondToGlideOrReflection tr det = glideOrReflectionCase m
--
  | otherwise = Left "?? (fromMatrix)"
  where
    tr = trace (rotPart m)
    det = detLU (rotPart m)

type Trace = Rational
type Determinant = Rational

-- Table 11.2.1.1. Identification of the type of the rotation part of the symmetry operation
correpondToRotInversion :: Trace -> Determinant -> Bool
correpondToRotInversion (-3) (-1) = True -- -1
correpondToRotInversion (-2) (-1) = True -- -6
correpondToRotInversion (-1) (-1) = True -- -4
correpondToRotInversion   0  (-1) = True -- -3
correpondToRotInversion   _    _  = False

correpondToNFoldRotation :: Trace -> Determinant -> Bool
correpondToNFoldRotation (-1) 1 = True -- 2
correpondToNFoldRotation   0  1 = True -- 3
correpondToNFoldRotation   1  1 = True -- 4
correpondToNFoldRotation   2  1 = True -- 6
correpondToNFoldRotation   _  _ = False

correpondToGlideOrReflection :: Trace -> Determinant -> Bool
correpondToGlideOrReflection 1 (-1) = True -- m
correpondToGlideOrReflection _   _  = False

-- 11.2.2. Derivation of coordinate triplets from symbols for symmetry operations

-- | 対称操作の幾何的表現の文字列から、行列表現を導出します。（cubic, tetragonal, orthorhombic, monoclinic, triclinic or rhombohedral）
-- Table 11.2.2.1を参照しています。
-- 返値は3x4行列
--
-- >>> prettyXYZ . toMatrixHex $ "-3+ 0,0,z; 0,0,0"
-- "y,-x,-z"
--
toMatrix = toMatrix' False

-- | 対称操作の幾何的表現の文字列をパースし、seiz matrixに変換します。(hexagonal)
-- Table 11.2.2.2を参照しています。
-- 返値は3x4行列
--
-- >>> prettyXYZ . toMatrixHex $ "-3+ 0,0,z; 0,0,0"
-- "y,y-x,-z"
--
toMatrixHex = toMatrix' True

-- Table 11.2.2.1と、Table 11.2.2.2、二つのテーブルの参照をBoolで切り替えています。
toMatrix' hex st = do
    a@(symbol,sense,vector,orientation) <- parseSymmetryOperation st
    -- Symbol of symmetry operation and orientation of symmetry elementを照らし合わせ、
    -- matrix Wと等価のTransformed coordinatesを得ます。
    transCoords <- lookup' symbol sense orientation
    -- Symbolがa b cだった場合、glide vectorが省略されているので復元します
    let vector' = fromMaybe vector . fromSymbol $ symbol
    -- 計算します。
    return $ calc symbol vector' orientation transCoords
    where
      lookup' = if hex then lookupMatrixWHex else lookupMatrixW
      fromSymbol "a" = Just "1/2,0,0"
      fromSymbol "b" = Just "0,1/2,0"
      fromSymbol "c" = Just "0,0,1/2"
      fromSymbol _   = Nothing

-- 計算部分
calc symbol vector orientation transCoords
 -- 下の部分で計算したwを参照で得たmatrixWの回転操作部分と連結します
 = rotPart matrixW <|> (modulo1 <$> w)
  where
    matrixW = fromXYZ'' transCoords
    -- wl = (I-W)x の計算をします
    wl = multStd (iw matrixW) (fromVec orientation)
    wg = fromVec vector
    -- w = wl + wlの計算をします
    w | isNotInversions = elementwise (+) wl wg
      -- inversionの場合、参照論文に記載がないが、別の計算が必要となり、それが以下
      -- 試行錯誤でみつけたもので根拠となる論文をまだみつけていないが、
      -- (ii) (a)の逆算でこうなった記憶（一年前の試行錯誤 from 2018）
      | otherwise = multStd (iw matrixW) (fromVec vector)
    -- symbolがinversionの一種ではない場合、真となる
    isNotInversions = symbol `notElem` ["-1","-2","-3","-4","-6"]
    -- 0 <= x < 1 となるように剰余をとる
    modulo1 n = mod' n 1
    -- ベクトル表記から3x1行列を生成する
    fromVec = transPart . fromXYZ''

{--

[Reference]

W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets
listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

TH. HAHN. (2006), Printed symbols for symmetry elements
listed in International Tables for Crystallography (2006). Vol. A, Chapter 1.3, pp. 5–6.


--}
