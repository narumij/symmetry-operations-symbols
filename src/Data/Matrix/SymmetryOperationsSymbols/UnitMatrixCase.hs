module Data.Matrix.SymmetryOperationsSymbols.UnitMatrixCase (
  unitMatrixCase,
  ) where

import Data.Matrix
import Data.Matrix.SymmetryOperationsSymbols.Common

-- (i) The matrix W is the unit matrix:
-- unitMatrixCase :: Matrix Rational -> String
unitMatrixCase w
  | all (== 0) w' = Right " 1  "
  | otherwise = Right $ " t " ++ tripletParen w' ++ " "
  where
    w' = toList . transPart $ w

{--

[Reference]

W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets
listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

--}
