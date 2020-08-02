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
- matrix-as-xyz-0.1.1.3
- symmetry-operations-symbols-0.0.1.4
```

Edit dependencies part of package.yaml like below.

```
dependencies:
- base >= 4.7 && < 5
- matrix-as-xyz
- symmetry-operations-symbols
```

Then start repl.

```shell
% stack repl
```

Setup packages and load modules.

```haskell
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

## Tex test

$ \normalsize 1 $
$ \normalsize t(\small 0\normalsize ,\small \frac{1}{2}\normalsize ,\small \frac{1}{2}\normalsize ) $
$ \normalsize m\ x,0,z $
$ \normalsize 2\ 0,y,0 $
$ \normalsize 2(\small 0\normalsize ,\small \frac{1}{2}\normalsize ,\small 0\normalsize )\ 0,y,0 $
$ \normalsize c\ x,0,z $
$ \normalsize n(\small \frac{1}{2}\normalsize ,\small 0\normalsize ,\small \frac{1}{2}\normalsize )\ x,0,z $
$ \normalsize \overline{1}\ \small \frac{1}{4}\normalsize ,\small \frac{1}{4}\normalsize ,\small \frac{1}{4} $
$ \normalsize d(\small \frac{1}{4}\normalsize ,\small 0\normalsize ,\small \frac{1}{4}\normalsize )\ x,\small \frac{1}{8}\normalsize ,z $
$ \normalsize 4\small ^+\normalsize \ 0,0,z $
$ \normalsize 4\small ^-\normalsize \ 0,0,z $
$ \normalsize 4\small ^+\normalsize (\small 0\normalsize ,\small 0\normalsize ,\small \frac{1}{4}\normalsize )\ 0,0,z $
$ \normalsize 4\small ^-\normalsize (\small 0\normalsize ,\small 0\normalsize ,\small \frac{3}{4}\normalsize )\ 0,0,z $
$ \normalsize \overline{1}\ \small 0\normalsize ,\small 0\normalsize ,\small 0 $
$ \normalsize \overline{4}\small ^+\normalsize \ \small \frac{1}{2}\normalsize ,\small \frac{1}{4}\normalsize ,z\ ; \ \small \frac{1}{2}\normalsize ,\small \frac{1}{4}\normalsize ,\small \frac{3}{8} $
$ \normalsize \overline{4}\small ^-\normalsize \ 0,\small \frac{1}{4}\normalsize ,z\ ; \ \small 0\normalsize ,\small \frac{1}{4}\normalsize ,\small \frac{1}{8} $






## References

1. W. Fischer. and E. Koch. (2006), Derivation of symbols and coordinate triplets International Tables for Crystallography (2006). Vol. A, Chapter 11.2, pp. 812–816.

2. Wondratschek, H. & Neubu ̈ser, J. (1967). Determination of the symmetry elements of a space group from the ‘general positions’ listed in International Tables for X-ray Crystallography, Vol. I. Acta Cryst. 23, 349–352.

## License

See the [LICENSE](https://raw.githubusercontent.com/narumij/symmetry-operations-symbols/master/LICENSE)
file in the repository.
