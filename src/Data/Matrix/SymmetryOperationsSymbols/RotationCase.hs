module Data.Matrix.SymmetryOperationsSymbols.RotationCase (
  nFoldRotationCase,
  ) where

import Data.Ratio
import Data.List (transpose)
import Data.Matrix hiding (transpose)
import Data.Matrix.SymmetryOperationsSymbols.Solve
import Data.Matrix.SymmetryOperationsSymbols.Common
import Data.Matrix.AsXYZ

-- | Case (ii) (a) W corresponds to a rotoinversion
--
-- [Reference]
--
-- W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets
-- listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.
nFoldRotationCase :: Matrix Rational -> Either ErrorMessage String
nFoldRotationCase m = maybeToEither "?? (rotation)" $ arrange m <$> solvingEquation m

arrange :: Matrix Rational -> [Rational] -> String
arrange m ans | all (==0) screwPart = unwords [sym,prettyXYZ location]
              | otherwise = unwords [sym ++ tripletParen screwPart,prettyXYZ location]
  where
    s = if null . senseOf $ m then " " else senseOf m
    sym = " " ++ show (rotationType m) ++ s
    screwPart = wg m
    location = locationOf m <|> fromList 3 1 ans

--

-- for repl check
-- "-z,-x+1/2,y"
-- " 3+(-1/6,1/6,1/6) x+1/6,-x+1/6,-x"
testMat = fromList 3 4 [ 0,0,-1,0, -1,0,0,1%2, 0,1,0,0 ]

-- Table 11.2.1.1. Identification of the type of the rotation part of the symmetry operation
rotationType :: (Num a,Eq a,Integral b) => Matrix a -> b
rotationType m
  | tr == (-1) = 2
  | tr ==   0  = 3
  | tr ==   1  = 4
  | tr ==   2  = 6
  where tr = trace $ rotPart m

pow :: (Num a, Integral b) => Matrix a -> b -> Matrix a
pow m 0 = identity 3
pow m n = foldl1 multStd $ replicate (fromIntegral n) (rotPart m)

t :: (Num b, Eq b) => Matrix b -> [b]
t mat = f [ pow mat n | n <- [0..(pred r)] ]
  where
    r = rotationType mat
    w = toList . transPart $ mat
    g n m = toList $ multStd m (fromList 3 1 n)
    -- この操作が思い出せない、理解できない
    f l = map sum $ transpose $ map (g w) l

-- | (W,w)^n
wg :: (Fractional b, Eq b) => Matrix b -> [b]
wg mat = (/ fromIntegral r) <$> t mat
  where
    r = rotationType mat

wl :: (Fractional b, Eq b) => Matrix b -> [b]
wl mat = zipWith (-) (toList . transPart $ mat) (wg mat)

solvingEquation' :: Integral a => Matrix (Ratio a) -> Maybe [Ratio a]
solvingEquation' mat = solve (iw mat) (wl mat)

solvingEquation :: Integral a => Matrix (Ratio a) -> Maybe [Ratio a]
solvingEquation mat = do
  sol <- solvingEquation' mat
  adjustAnswerOnAxis mat sol

-- 検算。repl用
check mat sol = if toList (multStd (iw mat) (vec sol)) == wl mat
                then sol
                else error "General solution error."
    where
      vec l = fromList (length l) 1 l

{--

[Reference]
W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets
listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

--}
