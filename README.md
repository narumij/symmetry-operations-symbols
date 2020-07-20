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

## License

See the [LICENSE](https://github.com/narumij/symmetry-operations-symbols/LICENSE)
file in the repository.
