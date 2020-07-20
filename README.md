# symmetry-operations-symbols

Haskell Derivation of symbols and coordinate triplets Library

## Quickstart

```shell
% stack repl
```

```haskell
-- prepare
repl> :set -package matrix-as-xyz
repl> :m Data.Matrix.AsXYZ
repl> :l Data.Matrix.SymmetryOperationsSymbols

-- print
repl> fromMatrix' . fromXYZ $ "x,y,z"

" 1 "

-- print 
repl> fromMatrix' . fromXYZ $ "-x,-y,z"

" 2  0,0,z"
```

## References

1. W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

2. Wondratschek, H. & Neubu ̈ser, J. (1967). Determination of the symmetry elements of a space group from the ‘general positions’ listed in International Tables for X-ray Crystallography, Vol. I. Acta Cryst. 23, 349–352.

## License

See the [LICENSE](https://raw.githubusercontent.com/narumij/symmetry-operations-symbols/master/LICENSE)
file in the repository.
