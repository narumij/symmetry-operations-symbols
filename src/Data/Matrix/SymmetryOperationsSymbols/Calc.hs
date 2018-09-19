module Data.Matrix.SymmetryOperationsSymbols.Calc (
    deriveSymmetryOperation
  ) where

import Data.Ratio
import Data.Matrix
import Data.Fixed
import Data.Maybe
import Data.Matrix.SymmetryOperationsSymbols.Common

-- (<|>)の衝突を避けたくてParse.hsと分けてある

deriveSymmetryOperation :: (Monad m, Integral a)
                        => (SymbolSenseVectorOrientation -> m String)
                        -> SymbolSenseVectorOrientation
                        -> m (Matrix (Ratio a))
deriveSymmetryOperation lookupFunc elements = calcMatrix elements <$> lookupFunc elements

calcMatrix :: Integral a => SymbolSenseVectorOrientation -> String -> Matrix (Ratio a)
calcMatrix (symbol,sense,vector,orientation) = calc symbol vector' orientation
    where
      vector' = fromMaybe vector . fromSymbol $ symbol
      fromSymbol "a" = Just "1/2,0,0"
      fromSymbol "b" = Just "0,1/2,0"
      fromSymbol "c" = Just "0,0,1/2"
      fromSymbol _   = Nothing

-- 対称操作の幾何表現から行列表現へ変換する際の計算部分
calc :: Integral a => String -> String -> String -> String -> Matrix (Ratio a)
calc symbol vector orientation transCoord
   -- 下の部分で計算したwを参照で得たmatrixWの回転操作部分と連結します
   = rotPart matrixW <|> (modulo1 <$> w)
    where
      matrixW = fromXYZ'' transCoord
      -- wl = (I-W)x の計算をします
      wl = multStd (iw matrixW) (fromVec orientation)
      wg = fromVec vector
      -- w = wl + wgの計算をします
      w | isNotInversions = elementwise (+) wl wg
        -- inversionの場合、参照論文に記載がないが、別の計算が必要となり、それが以下
        -- 試行錯誤でみつけたもので根拠となる論文をまだみつけていないが、
        -- (ii) (a)の逆算でこうなった記憶
        | otherwise = multStd (iw matrixW) (fromVec vector)
      -- symbolがinversionの一種ではない場合、真となる
      isNotInversions = symbol `notElem` ["-1","-2","-3","-4","-6"]
      -- 0 <= x < 1 となるように剰余をとる
      modulo1 n = mod' n 1
      -- ベクトル表記から3x1行列を生成する
      fromVec = transPart . fromXYZ''
