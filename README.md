# symmetry-operations-symbols

Haskell Derivation of symbols and coordinate triplets Library

## Quickstart

Make new stack project and move to project directory.

```shell
% stack new symopRepl
% cd symopRepl
```

Edit extra-deps part of stack.yaml like below.

```
extra-deps:
- matrix-as-xyz-0.1.1.1
- symmetry-operations-symbols-0.0.1.2
```

Then start repl.

```shell
% stack repl
```

Setup packages and load modules.

```haskell
-- prepare
repl> :set -package hall-symbols
repl> :set -package symmetry-operations-symbols
repl> :m Data.Matrix.AsXYZ Data.Matrix.SymmetryOperationsSymbols
```

Use like below.

```haskell
repl> fromMatrix' . fromXYZ $ "x,y,z"
" 1 "

repl> fromMatrix' . fromXYZ $ "-x,-y,z"
" 2  0,0,z"

repl> fromMatrix' . fromXYZ $ "-y,-x+1/2,z"
" g (-1/4,1/4,0) x+1/4,-x,z"
```

## References

1. W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

2. Wondratschek, H. & Neubu ̈ser, J. (1967). Determination of the symmetry elements of a space group from the ‘general positions’ listed in International Tables for X-ray Crystallography, Vol. I. Acta Cryst. 23, 349–352.

## License

See the [LICENSE](https://raw.githubusercontent.com/narumij/symmetry-operations-symbols/master/LICENSE)
file in the repository.
