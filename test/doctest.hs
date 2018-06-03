import Test.DocTest

main = doctest [
  "src/Data/Matrix/SymmetryOperationsSymbols.hs",
  "src/Data/Matrix/SymmetryOperationsSymbol/Common.hs"
  ]
