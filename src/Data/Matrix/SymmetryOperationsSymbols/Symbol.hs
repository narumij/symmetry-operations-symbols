module Data.Matrix.SymmetryOperationsSymbols.Symbol (
    Symbol(..)
    , lookupSymbolLabel
  ) where

import Control.Monad

data Symbol
   = Id -- '1'
   | T -- t
   | Inv -- '-1'
   | M -- 'm'
   | A -- a
   | B -- b
   | C -- c
   | D -- d
   | G -- g
   | N -- n
   | R2  -- '2'
   | R3  -- '3'
   | R4  -- '4'
   | R6  -- '6'
   | RI3 -- '-3'
   | RI4 -- '-4'
   | RI6 -- '-6'
   deriving (Show)

readId n = do
  ("1",st) <- lex n
  return (Id,st)

readT n = do
  ("t",st) <- lex n
  return (T,st)
  
readInv n = do
  ("-",nn) <- lex n
  ("1",st) <- lex nn
  return (Inv,st)

readM n = do
  ("m",st) <- lex n
  return (M,st)

readA n = do
  ("a",st) <- lex n
  return (A,st)

readB n = do
  ("b",st) <- lex n
  return (B,st)

readC n = do
  ("c",st) <- lex n
  return (C,st)

readD n = do
  ("d",st) <- lex n
  return (D,st)

readG n = do
  ("g",st) <- lex n
  return (G,st)

readN n = do
  ("n",st) <- lex n
  return (N,st)

readR2 n = do
  ("2",st) <- lex n
  return (R2,st)

readR3 n = do
  ("3",st) <- lex n
  return (R3,st)

readR4 n = do
  ("4",st) <- lex n
  return (R4,st)

readR6 n = do
  ("6",st) <- lex n
  return (R6,st)

readRI3 n = do
  ("-",nn) <- lex n
  ("3",st) <- lex nn
  return (RI3,st)
  
readRI4 n = do
  ("-",nn) <- lex n
  ("4",st) <- lex nn
  return (RI4,st)
  
readRI6 n = do
  ("-",nn) <- lex n
  ("6",st) <- lex nn
  return (RI6,st)

instance Read Symbol where
  readsPrec _ = join . sequence readSymbols
    where
      readSymbols = [
        readId, readT,
        readM, readA, readB, readC, readD, readG, readN,
        readR2, readR3, readR4, readR6,
        readInv, readRI3, readRI4, readRI6
        ]

                                            
fromSymbol :: Symbol -> String
fromSymbol Id = "1"
fromSymbol T = "t"
fromSymbol Inv = "-1"
fromSymbol M = "m"
fromSymbol A = "a"
fromSymbol B = "b"
fromSymbol C = "c"
fromSymbol D = "d"
fromSymbol G = "g"
fromSymbol N = "n"
fromSymbol R2 = "2"
fromSymbol R3 = "3"
fromSymbol R4 = "4"
fromSymbol R6 = "6"
fromSymbol RI3 = "-3"
fromSymbol RI4 = "-4"
fromSymbol RI6 = "-6"

lookupSymbol :: Symbol -> Symbol
lookupSymbol T = Id
lookupSymbol A = M
lookupSymbol B = M
lookupSymbol C = M
lookupSymbol D = M
lookupSymbol N = M
lookupSymbol G = M
lookupSymbol otherSymbol = otherSymbol

lookupSymbolLabel :: Symbol -> String
lookupSymbolLabel = fromSymbol . lookupSymbol

