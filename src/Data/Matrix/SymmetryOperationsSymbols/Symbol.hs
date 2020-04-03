module Data.Matrix.SymmetryOperationsSymbols.Symbol (
    Symbol
    , toLookup
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

instance Read Symbol where
  readsPrec _ = join . sequence [a,b,c,d,e,f,g,h,i,j,k,l,m,o,p,q,r]
    where
      a n = do
        ("1",st) <- lex n
        return (Id,st)
      b n = do
        ("-1",st) <- lex n
        return (Inv,st)
      c n = do
        ("m",st) <- lex n
        return (M,st)
      d n = do
        ("2",st) <- lex n
        return (R2,st)
      e n = do
        ("3",st) <- lex n
        return (R3,st)
      f n = do
        ("4",st) <- lex n
        return (R4,st)
      g n = do
        ("6",st) <- lex n
        return (R6,st)
      h n = do
        ("-3",st) <- lex n
        return (RI3,st)
      i n = do
        ("-4",st) <- lex n
        return (RI4,st)
      j n = do
        ("-6",st) <- lex n
        return (RI6,st)
      k n = do
        ("t",st) <- lex n
        return (T,st)
      l n = do
        ("a",st) <- lex n
        return (T,st)
      m n = do
        ("b",st) <- lex n
        return (T,st)
      o n = do
        ("c",st) <- lex n
        return (T,st)
      p n = do
        ("d",st) <- lex n
        return (T,st)
      q n = do
        ("g",st) <- lex n
        return (T,st)
      r n = do
        ("n",st) <- lex n
        return (T,st)
                                            
toShow :: Symbol -> String
toShow Id = "1"
toShow T = "t"
toShow Inv = "-1"
toShow M = "m"
toShow A = "a"
toShow B = "b"
toShow C = "c"
toShow D = "d"
toShow G = "g"
toShow N = "n"
toShow R2 = "2"
toShow R3 = "3"
toShow R4 = "4"
toShow R6 = "6"
toShow RI3 = "-3"
toShow RI4 = "-4"
toShow RI6 = "-6"

toLookup :: Symbol -> String
toLookup Id = "1"
toLookup T = "t"
toLookup Inv = "-1"
toLookup M = "m"
toLookup A = "m"
toLookup B = "m"
toLookup C = "m"
toLookup D = "m"
toLookup N = "m"
toLookup G = "m"
toLookup R2 = "2"
toLookup R3 = "3"
toLookup R4 = "4"
toLookup R6 = "6"
toLookup RI3 = "-3"
toLookup RI4 = "-4"
toLookup RI6 = "-6"

