module Data.Matrix.SymmetryOperationsSymbols.Parse (
  parseSymmetryOperation
  ) where

import Control.Monad
import Data.Maybe
import Data.List
import Text.ParserCombinators.Parsec

import TestData

sign :: CharParser () Char
sign = oneOf "-+"

zero :: CharParser () String
zero = do
  char '0'
  return "0"

num :: CharParser () String
num = do
  x <- oneOf "123456789"
  xs <- many digit
  return $ x : xs

int :: CharParser () String
int = zero <|> num

integer :: CharParser () String
integer = int

fract :: CharParser () String
fract = do
  n <- int
  optionSpaces
  char '/'
  optionSpaces
  d <- int
  return $ n ++ "/" ++ d

number :: CharParser () String
number =   try fract
      <|> integer

optionSpaces :: CharParser () ()
optionSpaces = option () spaces

vector :: CharParser () String
vector = do
  n1 <- num
  char ','
  n2 <- num
  char ','
  n3 <- num
  return $ intercalate "," [n1,n2,n3]
  where
    num = do
      optionSpaces
      s <- optionMaybe sign
      n1 <- number
      optionSpaces
      return $ maybe n1 (:n1) s

elementBody :: CharParser () String
elementBody = do
  n <- optionMaybe number
  optionSpaces
  v <- optionMaybe (oneOf "xyzXYZ")
  optionSpaces
  guard (isJust n || isJust v)
  return $ fromMaybe "" n ++ maybe "" (:[]) v

one :: CharParser () String
one = do
  s <- optionMaybe sign
  optionSpaces
  n <- elementBody
  return $ maybe n (:n) s

other :: CharParser () String
other = do
  s <- sign
  optionSpaces
  l <- elementBody
  return $ s:l

component :: CharParser () String
component = do
  optionSpaces
  x <- one
  xs <- many other
  optionSpaces
  return $ join (x:xs)

matrix :: CharParser () String
matrix = do
  n1 <- component
  char ','
  n2 <- component
  char ','
  n3 <- component
  return $ intercalate "," [n1,n2,n3]

parenVector :: CharParser () String
parenVector = do
  char '('
  v <- vector
  char ')'
  return v

elementFull :: CharParser () (String,String)
elementFull = do -- (x,x,x) n,n,n
  v <- parenVector
  optionSpaces
  m <- matrix
  return (v,m)

elementHalf :: CharParser () (String,String)
elementHalf = do -- n,n,n
  m <- matrix
  return ("",m)

element :: CharParser () (String,String)
element = try elementFull <|> elementHalf

identity :: CharParser () (String,String,String,String)
identity = do
  optionSpaces
  char '1'
  optionSpaces
  return ( "1", "", "", "" )

transform :: CharParser () (String,String,String,String)
transform = do
  optionSpaces
  char 't'
  optionSpaces
  vec <- parenVector
  return ( "t", "", vec, "" )

inversion :: CharParser () (String,String,String,String)
inversion = do
  optionSpaces
  string "-1"
  optionSpaces
  vec <- vector
  optionSpaces
  return ( "-1", "", vec, "" )

miller :: CharParser () (String,String,String,String)
miller = do
  optionSpaces
  sy <- oneOf "abcm"
  optionSpaces
  ori <- matrix
  optionSpaces
  return ( [sy], "", "", ori )

glide :: CharParser () (String,String,String,String)
glide = do
  optionSpaces
  sy <- oneOf "ndg"
  optionSpaces
  (vec,ori) <- element
  optionSpaces
  return ( [sy], "", vec, ori )

millerOrGlide :: CharParser () (String,String,String,String)
millerOrGlide = try miller <|> glide

rotation :: CharParser () (String,String,String,String)
rotation = do
  optionSpaces
  sy <- oneOf "2436"
  se <- optionMaybe sign
  optionSpaces
  (vec,ori) <- element
  optionSpaces
  return ( [sy],maybe "" (:[]) se, vec, ori )

invRotation :: CharParser () (String,String,String,String)
invRotation = do
  optionSpaces
  char '-'
  c <- oneOf "2436"
  se <- sign
  optionSpaces
  ori <- matrix
  optionSpaces
  char ';'
  optionSpaces
  vec <- vector
  optionSpaces
  return ( ['-',c], [se], vec, ori )

geometricRepresentation' :: CharParser () (String,String,String,String)
geometricRepresentation'
  = try identity
  <|> try transform
  <|> try inversion
  <|> try millerOrGlide
  <|> try rotation
  <|> invRotation

geometricRepresentation :: CharParser () (String,String,String,String)
geometricRepresentation = do
  a <- geometricRepresentation'
  optionSpaces
  eof
  return a

lefts   :: [Either a b] -> [a]
lefts x = [a | Left a <- x]
tests = map snd testData

parseSymmetryOperation' :: SourceName -> Either ParseError (String,String,String,String)
parseSymmetryOperation' st = parse geometricRepresentation st st

parseSymmetryOperation :: String -> Maybe (String,String,String,String)
parseSymmetryOperation st = case parseSymmetryOperation' st of
  Left _ -> Nothing
  Right s -> Just s
