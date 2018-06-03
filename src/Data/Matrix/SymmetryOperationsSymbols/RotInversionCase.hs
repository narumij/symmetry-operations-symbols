{-|
Module      : RotInversionCase
Copyright   : (c) Jun Narumi, 2018
License     : BSD-3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

[Reference]

W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets

listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

-}
module Data.Matrix.SymmetryOperationsSymbols.RotInversionCase (
  rotInversionCase,
  ) where

import Data.List
import Data.Maybe
import Data.Ratio
import Data.Matrix
import Data.Matrix.SymmetryOperationsSymbols.Solve
import Data.Matrix.SymmetryOperationsSymbols.Common
import Data.Matrix.AsXYZ

-- | Case (ii) (b) W corresponds to an n-fold rotation
rotInversionCase :: Matrix Rational -> Either ErrorMessage String
rotInversionCase m = maybeToEither "?? (rot inversion)" $ arrange m <$> calc m

calc m = (,) <$> solvingEquation2 m <*> solvingEquation1 m

arrange m (a,b) = unwords $ [symbol] ++ loc' ++ [triplet b]
  where
    rt = rotationType m
    symbol | rt == -1 = "-1 "
           | otherwise = show (rotationType m) ++ senseOf m
    loc' | null a = []
         | otherwise = [prettyXYZ loc ++ ";"]
    loc = locationOf m <|> fromList 3 1 a

------------

-- for repl check
-- "-z,-x+1/2,y"
-- " 3+(-1/6,1/6,1/6) x+1/6,-x+1/6,-x"
testMat = fromList 3 4 [ 0,0,1,0, 0,-1,0,1%2, -1,0,0,1%2 ]

-- Table 11.2.1.1. Identification of the type of the rotation part of the symmetry operation
rotationType :: (Num a,Eq a,Integral b) => Matrix a -> b
rotationType m
  | tr == (-3) = -1
  | tr == (-2) = -6
  | tr == (-1) = -4
  | tr ==   0  = -3
  where
    tr = trace $ rotPart m

wg :: (Fractional b, Eq b) => Matrix b -> Matrix b
wg mat = multStd (rotPart mat) (transPart mat)

wl :: (Fractional b, Eq b) => Matrix b -> Matrix b
wl mat = elementwise (+) (wg mat) (transPart mat)

-- (ii) (a) solving equation (W,w)x = x
solvingEquation1 :: Integral a => Matrix (Ratio a) -> Maybe [Ratio a]
solvingEquation1 mat = solve (iw mat) (toList . transPart $ mat)

-- (ii) (a) solving equation (W,w)^2 x = x
solvingEquation2' :: Integral a => Matrix (Ratio a) -> Maybe [Ratio a]
solvingEquation2' mat = solve (iw w2) (toList . wl $ mat)
  where
    pow2 m = multStd m m
    w2 = pow2 (rotPart mat)

solvingEquation2 :: Integral a => Matrix (Ratio a) -> Maybe [Ratio a]
solvingEquation2 mat = do
  sol <- solvingEquation2' mat
  adjustAnswerOnAxis mat sol

-- 検算。repl用
check mat sol = if multStd (iw2 mat) (vec sol) == wl mat
                then Right sol
                else Left "General solution error."
    where
      vec l = fromList (length l) 1 l
      iw2 m = iw $ multStd _W _W
      _W = rotPart mat

{--

[Reference]
W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets
listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

--}
