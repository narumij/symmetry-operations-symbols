{-|
Module      : GlideOrReflectionCase
Copyright   : (c) Jun Narumi, 2018
License     : BSD-3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

[Reference]

W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets

listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

-}
module Data.Matrix.SymmetryOperationsSymbols.GlideOrReflectionCase (
  glideOrReflectionCase
  ) where

import Control.Monad

import Data.Ratio
import Data.List (transpose)
import Data.Matrix hiding (transpose)
import Data.Matrix.SymmetryOperationsSymbols.Solve
import Data.Matrix.SymmetryOperationsSymbols.Common
import Data.Matrix.AsXYZ


-- | Case (ii) (c) W corresponds to a (glide) reflection
--
-- [Reference]
--
-- W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets
-- listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.
glideOrReflectionCase :: Matrix Rational -> Either ErrorMessage String
glideOrReflectionCase m = maybeToEither "?? (glide or reflection)" $ arrange m <$> solvingEquation m

arrange m ans
  | sym `elem` [M,A,B,C] = unwords [" " ++ show sym, " " ++ prettyXYZ location]
  | otherwise = unwords [" " ++ show sym, tripletParen glidePart, prettyXYZ location]
  where
    glidePart = wg m
    sym = millerSymbol (orientationOf m) glidePart
    location = locationOf m <|> fromList 3 1 ans

-- for repl check
-- "-y+1/2,-x,z+3/4"
-- " d (1/4,-1/4,3/4) x+1/4,-x,z"
testMat = fromList 3 4 [0,-1,0,1%2, -1,0,0,0, 0,0,1,3%4]

t :: (Fractional b, Eq b) => Matrix b -> [b]
t mat = f [ identity 3, rotPart mat ]
  where
    w = toList . transPart $ mat
    g n m = toList $ multStd m (fromList 3 1 n)
    -- この操作が思い出せない、理解できない
    f l = map sum $ transpose $ map (g w) l

-- glide part
wg :: (Fractional b, Eq b) => Matrix b -> [b]
wg mat = (/2) <$> t mat

wl :: (Fractional b, Eq b) => Matrix b -> [b]
wl mat = zipWith (-) (toList . transPart $ mat) (wg mat)

-- solving equation (W,wl)x = x
solvingEquation' :: Integral a => Matrix (Ratio a) -> Maybe [Ratio a]
solvingEquation' mat = solve (iw mat) (wl mat)

-------------------

solvingEquation :: Integral a => Matrix (Ratio a) -> Maybe [Ratio a]
solvingEquation mat = solvingEquation' mat >>= \ans -> adjustAnswerOnPlane mat ans

normalOf :: (Integral a,Num b) => Matrix (Ratio a) -> [b]
normalOf = axisOf

adjustAnswerOnPlane :: Integral a => Matrix (Ratio a) -> [Ratio a] -> Maybe [Ratio a]
adjustAnswerOnPlane mat s = intersectSegmentAndPlane (answerAdjustSegment mat) (plane (normalOf mat) s)

type Point a = [a]
type Vector a = [a]
type Plane a = (Vector a,a)
type Segment a = (Point a,Point a)

dot :: (Num a) => Vector a -> Vector a -> a
dot vecA vecB = sum $ zipWith (*) vecA vecB

plane :: (Num a) => Vector a -> Point a -> Plane a
plane normal point = (normal,d)
  where
    d = dot normal point

intersectSegmentAndPlane' :: (Ord c, Fractional c) => Segment c -> Plane c -> Point c
intersectSegmentAndPlane' (pointA,pointB) (planeN,planeD) = zipWith (+) pointA (map (*t) ab)
  where
    ab = zipWith (-) pointB pointA
    t = (planeD - dot planeN pointA) / dot planeN ab

-- 検算。repl用
check :: (Ord c, Fractional c) => Segment c -> Plane c -> Bool
check (pointA,pointB) (planeN,planeD) = t' /= 0 && t >= 0 && t <= 1.0
  where
    ab = zipWith (-) pointB pointA
    t' = dot planeN ab
    t = (planeD - dot planeN pointA) / dot planeN ab

intersectSegmentAndPlane :: (Ord c, Fractional c) => Segment c -> Plane c -> Maybe [c]
intersectSegmentAndPlane s@(pointA,pointB) p@(planeN,planeD)
  = if check s p
    then Just $ intersectSegmentAndPlane' s p
    else Nothing

answerAdjustSegment :: (Integral a, Num b) => Matrix (Ratio a) -> Segment b
answerAdjustSegment mat = (base, fmap negate base)
  where
    base = case axisOf mat of
      [ 0, 0, 1] -> [0,0,1]
      [ 0, 1, 0] -> [0,1,0]
      [ 1, 0, 0] -> [1,0,0]
      [ 1, 1, 0] -> [1,0,0]
      [ 1, 0, 1] -> [1,0,0]
      [ 0, 1, 1] -> [0,1,0]
      [ 1,-1, 0] -> [1,0,0]
      [-1, 0, 1] -> [1,0,0]
      [ 0, 1,-1] -> [0,1,0]
      [ 1, 2, 0] -> [1,0,0]
      [ 2, 1, 0] -> [1,0,0]
      --
      [ 2,-1, 0] -> [1,0,0]
      [-1, 2, 0] -> [1,0,0]
      a -> error $ "unknown normal of glide plane" ++ show a

--------------

data GlideType = M | A | B | C | N | D | G deriving Eq

instance Show GlideType where
          show M = "m"
          show A = "a"
          show B = "b"
          show C = "c"
          show N = "n"
          show D = "d"
          show G = "g"

type Orientation a = [Ratio a]
type GlideVector a = [Ratio a]

-- 1.3.1. Printed symbols for symmetry elements and for the corresponding symmetry operations in one, two and three dimensions
millerSymbol :: (Integral a) => Orientation a -> GlideVector a -> GlideType
millerSymbol o v
  | v == [0,0,0] = M
  -----
  | o `elem` [ [0,1,0],  [0,0,1] ] && v == [1%2,0,0] = A
  -----
  | o `elem` [ [0,0,1],  [1,0,0] ] && v == [0,1%2,0] = B
  -----
  | o `elem` [ [1,0,0],  [0,1,0] ] && v == [0,0,1%2] = C
  | o `elem` [ [1,-1,0], [1,1,0] ] && v == [0,0,1%2] = C
  | o `elem` [ [1,0,0],  [0,1,0],  [-1,-1,0] ] && v == [0,0,1%2] = C
  | o `elem` [ [1,-1,0], [1,2,0],  [2,1,0]   ] && v == [0,0,1%2] = C
  -----
  | o `elem` [ [0,0,1],  [1,0,0],  [0,1,0]   ] && v == [1%2,1%2,0] = N
  | o `elem` [ [1,-1,0], [0,1,-1], [-1,0,1]  ] && v == [1%2,1%2,1%2] = N
  | o == [1,1,0] && v == [-1%2,1%2,1%2]  = N
  | o == [0,1,1] && v == [1%2,-1%2,1%2] = N
  | o == [1,0,1] && v == [1%2,1%2,-1%2] = N
  -------
  | o == [0,0,1] && vv [_14,_14w,_0] = D
  | o == [1,0,0] && vv [_0,_14,_14w] = D
  | o == [0,1,0] && vv [_14w,_0,_14] = D
  | o == [1,-1,0] && vv [_14,_14,_14w] = D
  | o == [0,1,-1] && vv [_14w,_14,_14] = D
  | o == [-1,0,1] && vv [_14,_14w,_14] = D
  | o == [1,1,0] && vv [_14n,_14,_14w] = D
  | o == [0,1,1] && vv [_14w,_14n,_14] = D
  | o == [1,0,1] && vv [_14,_14w,_14n] = D

  ------- 以下、該当する条件がみつからかったものについて、照らし合わせながら試行錯誤した結果
  | ( o `elem` [ [0,1,1], [0,1,-1], [0,-1,1], [0,-1,-1] ] ) && v == [1%2,0,0] = A -- ?
  | ( o `elem` [ [1,0,1], [1,0,-1], [-1,0,1], [-1,0,-1] ] ) && v == [0,1%2,0] = B -- ?
  | o == [0,1,0] && v == [1%2,0,1%2] = N
  | o == [1,0,0] && v == [0,1%2,1%2] = N
  | o == [0,0,1] && v == [1%2,1%2,0] = N -- ?
  | o == [0,0,1] && vv [_14w,_14w,_0] = D
  | o == [1,0,0] && vv [_0,_14w,_14w] = D
  | o == [0,1,0] && vv [_14w,_0,_14w] = D
  | o == [1,1,0] && vv [_14,_14n,_14w] = D
  | o == [0,1,1] && vv [_14w,_14,_14n] = D
  | o == [1,0,1] && vv [_14n,_14w,_14] = D
  | o `elem` [[1,-1,0],[-1,0,1],[0,1,-1]] && vv [_34,_34,_34] = D
  ---------
  | otherwise = G
  where
    _14 n = n == 1%4
    _14n n = n == -1%4
    _14v n = abs n == 1%4
    _14w n = n == 1%4 || n == 3%4
    _34 n = n == 3%4
    _0 n = n == 0
    vv p = all (\(f,x)->f x) $ zip p v

{--

[Reference]

W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets
listed in International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

TH. HAHN. (2006), Printed symbols for symmetry elements
listed in International Tables for Crystallography (2006). Vol. A, Chapter 1.3, pp. 5–6.


--}
