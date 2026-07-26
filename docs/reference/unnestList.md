# Un-nest a nested list into a simple list

Un-nest a nested list into a simple list

## Usage

``` r
unnestList(
  x,
  addNames = FALSE,
  unnamedBase = "x",
  parentName = NULL,
  sep = ".",
  makeNamesFunc = makeNames,
  stopClasses = c("dendrogram", "data.frame", "matrix", "package_version", "tbl",
    "data.table"),
  extraStopClasses = getOption("jam.stopClasses"),
  ...
)
```

## Arguments

- x:

  `list` potentially containing nested lists.

- addNames:

  `logical` indicating whether to add names to the list elements when
  names are not already present. When `addNames=TRUE` and no names are
  present `unnamedBase` is used to define names.

- unnamedBase:

  `character` value used as a base for naming any un-named lists, using
  the format `makeNamesFunc(rep(unnamedBase, n))`.

- parentName:

  `character` with optional prefix, used as parent name, default is
  NULL.

- sep:

  `character` delimiter used between nested list names.

- makeNamesFunc:

  `function` that takes a character vector and returns non-duplicated
  character vector of equal length. By default it uses
  [`jamba::makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md).

- stopClasses:

  `vector` of classes that should not be un-nested, useful in case some
  classes inherit list properties.

- extraStopClasses:

  `vector` of additional values for `stopClasses`, created mostly to
  show that `options("jam.stopClasses")` can be used to define
  `stopClasses`, for example when this function is called but where
  arguments cannot be conveniently passed through the calling function.

- ...:

  additional arguments are ignored.

## Value

`list` that has been flattened so that it contains no `list` elements.
Note that it may contain some list-like objects such as `data.frame`,
defined by `stopClasses`.

## Details

This function inspects a list, and unlists each entry resulting in a
simple list of non-list entries as a result. Sometimes when
concatenating lists together, one list gets added as a list-of-lists.
This function resolves that problem by providing one flat list.

## See also

Other jam list functions:
[`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md),
[`heads()`](https://jmw86069.github.io/jamba/reference/heads.md),
[`jam_rapply()`](https://jmw86069.github.io/jamba/reference/jam_rapply.md),
[`list2df()`](https://jmw86069.github.io/jamba/reference/list2df.md),
[`mergeAllXY()`](https://jmw86069.github.io/jamba/reference/mergeAllXY.md),
[`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md),
[`rbindList()`](https://jmw86069.github.io/jamba/reference/rbindList.md),
[`relist_named()`](https://jmw86069.github.io/jamba/reference/relist_named.md),
[`rlengths()`](https://jmw86069.github.io/jamba/reference/rlengths.md),
[`sclass()`](https://jmw86069.github.io/jamba/reference/sclass.md),
[`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md),
[`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md)

## Examples

``` r
L <- list(A=letters[1:10],
   B=list(C=LETTERS[3:9], D=letters[4:11]),
   E=list(F=list(G=LETTERS[3:9], D=letters[4:11])));
L;
#> $A
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
#> 
#> $B
#> $B$C
#> [1] "C" "D" "E" "F" "G" "H" "I"
#> 
#> $B$D
#> [1] "d" "e" "f" "g" "h" "i" "j" "k"
#> 
#> 
#> $E
#> $E$F
#> $E$F$G
#> [1] "C" "D" "E" "F" "G" "H" "I"
#> 
#> $E$F$D
#> [1] "d" "e" "f" "g" "h" "i" "j" "k"
#> 
#> 
#> 

# inspect the data using str()
str(L);
#> List of 3
#>  $ A: chr [1:10] "a" "b" "c" "d" ...
#>  $ B:List of 2
#>   ..$ C: chr [1:7] "C" "D" "E" "F" ...
#>   ..$ D: chr [1:8] "d" "e" "f" "g" ...
#>  $ E:List of 1
#>   ..$ F:List of 2
#>   .. ..$ G: chr [1:7] "C" "D" "E" "F" ...
#>   .. ..$ D: chr [1:8] "d" "e" "f" "g" ...

unnestList(L);
#> $A
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
#> 
#> $B.C
#> [1] "C" "D" "E" "F" "G" "H" "I"
#> 
#> $B.D
#> [1] "d" "e" "f" "g" "h" "i" "j" "k"
#> 
#> $E.F.G
#> [1] "C" "D" "E" "F" "G" "H" "I"
#> 
#> $E.F.D
#> [1] "d" "e" "f" "g" "h" "i" "j" "k"
#> 

# optionally change the delimiter
unnestList(L, sep="|");
#> $A
#>  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
#> 
#> $`B|C`
#> [1] "C" "D" "E" "F" "G" "H" "I"
#> 
#> $`B|D`
#> [1] "d" "e" "f" "g" "h" "i" "j" "k"
#> 
#> $`E|F|G`
#> [1] "C" "D" "E" "F" "G" "H" "I"
#> 
#> $`E|F|D`
#> [1] "d" "e" "f" "g" "h" "i" "j" "k"
#> 

# example with nested lists of data.frame objects
df1 <- data.frame(a=1:2, b=letters[3:4]);
DFL <- list(A=df1,
   B=list(C=df1, D=df1),
   E=list(F=list(G=df1, D=df1)));
str(DFL);
#> List of 3
#>  $ A:'data.frame':   2 obs. of  2 variables:
#>   ..$ a: int [1:2] 1 2
#>   ..$ b: chr [1:2] "c" "d"
#>  $ B:List of 2
#>   ..$ C:'data.frame':    2 obs. of  2 variables:
#>   .. ..$ a: int [1:2] 1 2
#>   .. ..$ b: chr [1:2] "c" "d"
#>   ..$ D:'data.frame':    2 obs. of  2 variables:
#>   .. ..$ a: int [1:2] 1 2
#>   .. ..$ b: chr [1:2] "c" "d"
#>  $ E:List of 1
#>   ..$ F:List of 2
#>   .. ..$ G:'data.frame': 2 obs. of  2 variables:
#>   .. .. ..$ a: int [1:2] 1 2
#>   .. .. ..$ b: chr [1:2] "c" "d"
#>   .. ..$ D:'data.frame': 2 obs. of  2 variables:
#>   .. .. ..$ a: int [1:2] 1 2
#>   .. .. ..$ b: chr [1:2] "c" "d"
unnestList(DFL);
#> $A
#>   a b
#> 1 1 c
#> 2 2 d
#> 
#> $B.C
#>   a b
#> 1 1 c
#> 2 2 d
#> 
#> $B.D
#>   a b
#> 1 1 c
#> 2 2 d
#> 
#> $E.F.G
#>   a b
#> 1 1 c
#> 2 2 d
#> 
#> $E.F.D
#>   a b
#> 1 1 c
#> 2 2 d
#> 
str(unnestList(DFL));
#> List of 5
#>  $ A    :'data.frame':   2 obs. of  2 variables:
#>   ..$ a: int [1:2] 1 2
#>   ..$ b: chr [1:2] "c" "d"
#>  $ B.C  :'data.frame':   2 obs. of  2 variables:
#>   ..$ a: int [1:2] 1 2
#>   ..$ b: chr [1:2] "c" "d"
#>  $ B.D  :'data.frame':   2 obs. of  2 variables:
#>   ..$ a: int [1:2] 1 2
#>   ..$ b: chr [1:2] "c" "d"
#>  $ E.F.G:'data.frame':   2 obs. of  2 variables:
#>   ..$ a: int [1:2] 1 2
#>   ..$ b: chr [1:2] "c" "d"
#>  $ E.F.D:'data.frame':   2 obs. of  2 variables:
#>   ..$ a: int [1:2] 1 2
#>   ..$ b: chr [1:2] "c" "d"

# packageVersion() returns class "package_version"
# where is.list(packageVersion("base")) is TRUE,
# but it cannot ever be subsetted as a list with x[[1]],
# and thus it breaks this function
identical(is.list(packageVersion("base")), is.list(packageVersion("base"))[[1]])
#> [1] TRUE
unnestList(lapply(nameVector(c("base","graphics")), packageVersion))
#> $base
#> [1] '4.5.2'
#> 
#> $graphics
#> [1] '4.5.2'
#> 
```
