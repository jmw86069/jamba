# print dimensions of list object elements

`sdim()` prints the name and dimensions of `list` object elements, such
as a `list` of `data.frame`

`ssdim()` prints the name and dimensions of nested elements of `list`
objects, for example a `list` of `list` objects that each contain other
objects.

`sdima()` prints the name and dimensions of object `attributes(x)`. It
is useful for summarizing the
[`attributes()`](https://rdrr.io/r/base/attributes.html) of an object.

`ssdima()` prints the name and dimensions of nested elements of `list`
object [`attributes()`](https://rdrr.io/r/base/attributes.html), for
example a `list` of `list` objects that each contain other objects. It
is useful for comparing attributes across `list` elements.

This function prints the dimensions of a list of objects, usually a
`list` of `data.frame` objects, but extended to handle more complicated
lists, including even S4 object
[`methods::slotNames()`](https://rdrr.io/r/methods/slot.html).

Over time, more object types will be made compatible with this function.
Currently, `igraph` objects will print the number of nodes and edges,
but requires the igraph package to be installed.

## Usage

``` r
sdim(
  x,
  includeClass = TRUE,
  doFormat = FALSE,
  big.mark = ",",
  verbose = FALSE,
  ...
)

sdima(
  x,
  includeClass = TRUE,
  doFormat = FALSE,
  big.mark = ",",
  verbose = FALSE,
  ...
)

ssdima(
  x,
  includeClass = TRUE,
  doFormat = FALSE,
  big.mark = ",",
  verbose = FALSE,
  ...
)

ssdim(
  x,
  includeClass = TRUE,
  doFormat = FALSE,
  big.mark = ",",
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  one of several recognized object classes:

  - an S3 object inheriting from class `"list"`, including a nested list
    of lists or simple list

  - an S3 atomic object, which returns only the length

  - a single multi-dimensional object such as `data.frame`, `matrix`,
    `array`, `tibble`, or similar, which returns only its dimensions.

  - an `S4` object in which case it used `methods::slotNames(x)` to
    traverse the object structure

  - an `"environment"` object, in which case `ls(envir=x)` is used to
    traverse the object structure.

  - When the object is `S4` that inherits `"List"` from the `S4Vectors`
    package, it will attempt to use the proper subset functions from
    `S4Vectors` via `names(x)`, but that process only works properly if
    the `S4Vectors` package is previously loaded, otherwise it reverts
    to using `methods::slotNames(x)`.

- includeClass:

  `logical` indicating whether to print the class of each element in the
  input `x` object. Note that for S4 objects, each element will be the
  object returned for each of `methods::slotNames(x)`.

- doFormat:

  `logical` indicating whether to format the dimensions using
  `format(...,big.mark=",")`, which is mainly useful for extremely large
  dimensions. This parameter should probably become more broadly useful
  and respectful for different locales.

- big.mark:

  `character` value used when `doFormat=TRUE`, used in the call to
  `format(...,big.mark)`.

- verbose:

  `logical` whether to print verbose output

- ...:

  additional parameters are ignored.

## Value

`data.frame` where each row indicates the dimensions of each element in
the input list. When `includeClass` is `TRUE` it will include a column
`class` which indicates the class of each list element. When the input
list contains arrays with more than two dimensions, the first two
dimensions are named `"rows"` and `"columns"` with additional dimensions
named `"dim3"` and so on. Any list element with fewer than that many
dimensions will only have values populated to the relevant dimensions,
for example a character vector will only populate the length.

`data.frame` which describes the dimensions of the objects in
`attributes(x)`.

`list` of `data.frame` each of which describes the dimensions of the
objects in `attributes(x)`.

`list` of `data.frame`, each row indicates the dimensions of each
element in the input list. When `includeClass` is `TRUE` it will include
a column `class` which indicates the class of each list element. When
the input `list` contains arrays with more than two dimensions, the
first two dimensions are named `"rows"` and `"columns"` with additional
dimensions named `"dim3"` and so on. Any `list` element with fewer than
that many dimensions will only have values populated to the relevant
dimensions, for example a character vector will only populate the
length.

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
[`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md),
[`unnestList()`](https://jmw86069.github.io/jamba/reference/unnestList.md)

## Examples

``` r
L <- list(LETTERS=LETTERS,
   letters=letters,
   lettersDF=data.frame(LETTERS, letters));
sdim(L)
#>           rows cols      class
#> LETTERS     26       character
#> letters     26       character
#> lettersDF   26    2 data.frame

LL <- list(L=L, A=list(1:10))
sdim(LL)
#>   rows class
#> L    3  list
#> A    1  list
ssdim(LL)
#> $L
#>           rows cols      class
#> LETTERS     26       character
#> letters     26       character
#> lettersDF   26    2 data.frame
#> 
#> $A
#>   rows   class
#> 1   10 integer
#> 

m <- matrix(1:9,
   ncol=3,
   dimnames=list(
      Rows=letters[1:3],
      Columns=LETTERS[1:3]));
sdima(m);
#>          rows   class
#> dim         2 integer
#> dimnames    2    list
ssdima(m);
#> $dim
#>   rows   class
#> 1    2 integer
#> 
#> $dimnames
#>         rows     class
#> Rows       3 character
#> Columns    3 character
#> 
```
