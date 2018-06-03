{-|
Module      : UnitMatrixCase
Copyright   : (c) Jun Narumi, 2018
License     : BSD-3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

[Reference]

W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets

listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

-}

module Data.Matrix.SymmetryOperationsSymbols.UnitMatrixCase (
  unitMatrixCase,
  ) where

import Data.Matrix
import Data.Matrix.SymmetryOperationsSymbols.Common

-- | Case (i) The matrix W is the unit matrix:
unitMatrixCase :: Matrix Rational -> Either ErrorMessage String
unitMatrixCase w
  | all (== 0) w' = Right " 1  "
  | otherwise = Right $ " t " ++ tripletParen w' ++ " "
  where
    w' = toList . transPart $ w
