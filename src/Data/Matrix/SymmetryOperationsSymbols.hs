{-|
Module      : SymmetryOperationsSymbols
Copyright   : (c) Jun Narumi, 2018
License     : BSD-3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

[References]

W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets

listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

-}

module Data.Matrix.SymmetryOperationsSymbols (
  fromMatrix,
  fromMatrix',
  toMatrix,
  toMatrixHex,
  notHexagonal,
  hexagonal
  ) where

import Data.Ratio (Ratio)    
import Data.Matrix (Matrix,detLU,trace,identity)
import Text.ParserCombinators.Parsec (ParseError,parse)

import Data.Matrix.SymmetryOperationsSymbols.Common

import Data.Matrix.SymmetryOperationsSymbols.UnitMatrixCase
import Data.Matrix.SymmetryOperationsSymbols.GlideOrReflectionCase
import Data.Matrix.SymmetryOperationsSymbols.RotationCase
import Data.Matrix.SymmetryOperationsSymbols.RotInversionCase

import Data.Matrix.SymmetryOperationsSymbols.Parser
import Data.Matrix.SymmetryOperationsSymbols.SymmetryOperation -- (SymmetryOperation)
import Data.Matrix.SymmetryOperationsSymbols.Calc


-- for doctest
import Data.Matrix.AsXYZ

-- | 与えられた対称操作の行列から、対称操作の幾何的表現を導出します。
--
-- >>> fromMatrix . fromXYZ $ "x,y,z"
-- Right " 1 "
--
-- >>> fromMatrix . fromXYZ $ "-y,x,-z"
-- Right "-4- 0,0,z; 0,0,0"
--
fromMatrix :: Integral a =>
              Matrix (Ratio a) -- ^ 3x4 or 4x4 Matrix
           -> Either String String
fromMatrix = fromMatrix'

fromMatrix' :: (Monad m, Integral a) => Matrix (Ratio a) -> m String
fromMatrix' m = showSymmetryOperation <$> fromMatrix'' m

fromMatrix'' :: (Monad m, Integral a) => Matrix (Ratio a) -> m (SymmetryOperation a)
fromMatrix'' m
  -- (i)
  | rotPart m == identity 3             = unitMatrixCase m
  -- (ii) (a)
  | correpondToRotInversion tr det      = rotInversionCase m
  -- -- (ii) (b)
  | correpondToNFoldRotation tr det     = nFoldRotationCase m
  -- -- (ii) (c)
  | correpondToGlideOrReflection tr det = glideOrReflectionCase m
  -- --
  | otherwise = fail "matrix is not symmetry operation."
  where
  tr  = trace (rotPart m)
  det = detLU (rotPart m)

  
-- 11.2.2. Derivation of coordinate triplets from symbols for symmetry operations

-- | 対称操作の幾何的表現の文字列から行列表現の導出
--
-- for cubic, tetragonal, orthorhombic, monoclinic, triclinic or rhombohedral.
--
-- >>> prettyXYZ <$> toMatrixHex "-4- 0,0,z; 0,0,0"
-- Right "-y,x,-z"
--
toMatrix :: Integral a =>
            String -- ^ like " -1 0,0,0"
         -> Either ParseError (Matrix (Ratio a)) -- ^ 3x4 Matrix
toMatrix st = parse notHexagonal st st

-- | 対称操作の幾何的表現の文字列から行列表現の導出
--
-- for hexagonal.
--
-- >>> prettyXYZ <$> toMatrixHex "-3+ 0,0,z; 0,0,0"
-- Right "y,y-x,-z"
--
toMatrixHex :: Integral a =>
               String -- ^ like " -1 0,0,0"
            -> Either ParseError (Matrix (Ratio a)) -- ^ 3x4 Matrix
toMatrixHex st = parse hexagonal st st
