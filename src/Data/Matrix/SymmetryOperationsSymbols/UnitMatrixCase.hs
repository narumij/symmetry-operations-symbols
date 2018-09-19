{-|
Module      : UnitMatrixCase
Copyright   : (c) Jun Narumi, 2018
License     : BSD-3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

[References]

W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets

listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

-}

module Data.Matrix.SymmetryOperationsSymbols.UnitMatrixCase (
  unitMatrixCase,
  -- unitMatrixCase2,
  ) where

import Data.Ratio (Ratio)    
import Data.Matrix
import Data.Matrix.SymmetryOperationsSymbols.Common
import Data.Matrix.SymmetryOperationsSymbols.SymmetryOperation

-- | Case (i) The matrix W is the unit matrix:
unitMatrixCase w
    | all (== 0) w' = return Identity
    | otherwise     = return $ Translation { vector = (a,b,c) }
    where
      w'@[a,b,c] = toList . transPart $ w

