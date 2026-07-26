# rbind a list of vectors into matrix or data.frame

rbind a list of vectors into matrix or data.frame

## Usage

``` r
rbindList(
  x,
  emptyValue = "",
  nullValue = NULL,
  keepListNames = TRUE,
  newColnames = NULL,
  newRownames = NULL,
  fixBlanks = TRUE,
  returnDF = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  `list` of atomic `vector`, `matrix`, or `data.frame` objects.

- emptyValue:

  `character` value to use to represent missing values, whenever a blank
  cell is introduced into the resulting matrix

- nullValue:

  optional value used to replace NULL entries in the input list, useful
  especially when the data was produced by
  [`strsplit()`](https://rdrr.io/r/base/strsplit.html) with `""`. Use
  `nullValue=""` to replace 'NULL' with `""` and preserve the original
  list length. Otherwise when `nullValue=NULL` any empty entries will be
  silently dropped.

- keepListNames:

  `logical` whether to use list names as rownames in the resulting
  matrix or data.frame.

- newColnames:

  NULL or `character` vector of colnames to use for the resulting matrix
  or data.frame.

- newRownames:

  NULL or `character` vector of rownames to use for the resulting matrix
  or data.frame. If supplied, this value overrides the
  keepListNames=TRUE use of list names as rownames.

- fixBlanks:

  `logical` whether to use blank values instead of repeating each vector
  to the length of the maximum vector length when filling each row of
  the matrix or data.frame.

- returnDF:

  `logical` whether to return a data.frame, by default FALSE, a matrix
  is returned.

- verbose:

  `logical` whether to print verbose output during processing.

- ...:

  Additional arguments are ignored.

## Value

`matrix` unless `returnDF=TRUE` in which the output is coerced to a
`data.frame`. The rownames by default are derived from the list names,
but the colnames are not derived from the vector names. If input `x`
contains `data.frame` or `matrix` objects, the output will retain those
values.

## Details

The purpose of this function is to emulate `do.call(rbind, x)` on a list
of vectors, while specifically handling when there are different numbers
of entries per vector. The output `matrix` number of columns will be the
longest vector (or largest number of columns) in the input list `x`.

Instead of recycling values in each row to fill the target number of
columns, this function fills cells with blank fields, with default
argument `fixBlanks=TRUE`.

In extensive timings tests at the time this function was created, this
technique was notably faster than alternatives. It runs
`do.call(rbind, x)` then subsequently replaces recycled values with
blank entries, in a manner that is notably faster than alternative
approaches such as pre-processing the input data.

## See also

Other jam list functions:
[`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md),
[`heads()`](https://jmw86069.github.io/jamba/reference/heads.md),
[`jam_rapply()`](https://jmw86069.github.io/jamba/reference/jam_rapply.md),
[`list2df()`](https://jmw86069.github.io/jamba/reference/list2df.md),
[`mergeAllXY()`](https://jmw86069.github.io/jamba/reference/mergeAllXY.md),
[`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md),
[`relist_named()`](https://jmw86069.github.io/jamba/reference/relist_named.md),
[`rlengths()`](https://jmw86069.github.io/jamba/reference/rlengths.md),
[`sclass()`](https://jmw86069.github.io/jamba/reference/sclass.md),
[`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md),
[`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md),
[`unnestList()`](https://jmw86069.github.io/jamba/reference/unnestList.md)

## Examples

``` r
L <- list(a=LETTERS[1:4], b=letters[1:3]);
rbindList(L);
#>   [,1] [,2] [,3] [,4]
#> a "A"  "B"  "C"  "D" 
#> b "a"  "b"  "c"  ""  
rbindList(L, returnDF=TRUE);
#>   1 2 3 4
#> a A B C D
#> b a b c  
```
