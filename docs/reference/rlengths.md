# lengths for recursive lists

lengths for recursive lists

## Usage

``` r
rlengths(x, doSum = NULL, ...)
```

## Arguments

- x:

  `list` or vector

- doSum:

  `logical` indicating whether to return the overall sum of lengths.
  When 'NULL' it will return the aggregate length of each list element
  in `x`. When `FALSE` it will return the same list structure of x, with
  the length of each. When `TRUE` it will return the total length of all
  elements in `x` as one value.

- ...:

  additional parameters are ignored

## Value

`integer` value, vector, or list:

- When `doSum is NULL` (default) it returns an `integer` vector with
  length `length(x)` and names `names(x)`, whose values are the total
  number of elements in each item in `x` after running
  [`base::unlist()`](https://rdrr.io/r/base/unlist.html).

- When `doSum=="TRUE"`, it returns the single `integer` length of all
  elements in `x`.

- When `doSum=="FALSE"`, it returns the full structure of `x` with the
  `integer` length of each element.

The parameter `doSum` is intended for internal use, during recursive
calls of `rlengths()` to itself. When `doSum is NULL` or `TRUE`,
recursive calls to `rlengths()` set `doSum=TRUE`.

## Details

This function takes a list as input, and returns the length of each list
element after running
[`base::unlist()`](https://rdrr.io/r/base/unlist.html).

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
[`sclass()`](https://jmw86069.github.io/jamba/reference/sclass.md),
[`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md),
[`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md),
[`unnestList()`](https://jmw86069.github.io/jamba/reference/unnestList.md)

## Examples

``` r
x <- list(
   A=list(
      A1=nameVector(1:3, letters[1:3]),
      A2=list(
         A1a=nameVector(4:7, letters[4:7]),
         A1b=nameVector(11:14, letters[11:14]))),
   B=list(B1=nameVector(1:9, letters[1:9]),
      B2=nameVector(20:25, letters[20:25])));
# default lengths(x) shows length=2 for A and B
lengths(x)
#> A B 
#> 2 2 
# rlengths(x) shows the total length of A and B
rlengths(x)
#>  A  B 
#> 11 15 
```
