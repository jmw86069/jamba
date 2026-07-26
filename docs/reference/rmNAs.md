# remove NA values from list elements

remove NA values from list elements

## Usage

``` r
rmNAs(
  x,
  naValue = NULL,
  rmNULL = FALSE,
  nullValue = naValue,
  rmInfinite = TRUE,
  infiniteValue = NULL,
  rmNAnames = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  `list` of vectors

- naValue:

  NULL or single replacement value for NA entries. If NULL, then NA
  entries are removed from the result.

- rmNULL:

  `logical` whether to replace NULL entries with `nullValue`

- nullValue:

  NULL or single replacement value for NULL entries. If NULL, then NULL
  entries are removed from the result.

- rmInfinite:

  `logical` whether to replace Infinite values with infiniteValue

- infiniteValue:

  value to use when rmInfinite==TRUE to replace entries which are Inf or
  -Inf.

- rmNAnames:

  `logical` whether to remove entries which have NA as the name,
  regardless whether the entry itself is NA.

- verbose:

  `logical` whether to print verbose output

- ...:

  additional arguments are ignored.

## Value

`list` where NA entries were removed or replaced with `naValue` in each
vector. Empty `list` elements are optionally removed when `rmNULL=TRUE`,
or replaced with `nullValue` when defined. When `rmInfinite=TRUE` then
infinite values are either removed, or replaced with `infiniteValue`
when defined.

## Details

This function removes `NA` values from vectors in a `list`, applying the
same logic used in
[`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md) to each
vector. It is somewhat optimized, in that it checks for list elements
that have `NA` values before applying
[`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md). However,
it calls [`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md)
iteratively on each vector that contains `NA` in order to preserve the
class (factor, character, numeric, etc.) of each vector.

It also optionally applies convenience functions
[`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md) and
[`rmInfinite()`](https://jmw86069.github.io/jamba/reference/rmInfinite.md)
as relevant.

## See also

Other jam practical functions:
[`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md),
[`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md),
[`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md),
[`check_pkg_installed()`](https://jmw86069.github.io/jamba/reference/check_pkg_installed.md),
[`colNum2excelName()`](https://jmw86069.github.io/jamba/reference/colNum2excelName.md),
[`color_dither()`](https://jmw86069.github.io/jamba/reference/color_dither.md),
[`exp2signed()`](https://jmw86069.github.io/jamba/reference/exp2signed.md),
[`getAxisLabel()`](https://jmw86069.github.io/jamba/reference/getAxisLabel.md),
[`isFALSEV()`](https://jmw86069.github.io/jamba/reference/isFALSEV.md),
[`isTRUEV()`](https://jmw86069.github.io/jamba/reference/isTRUEV.md),
[`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md),
[`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md),
[`lldf()`](https://jmw86069.github.io/jamba/reference/lldf.md),
[`log2signed()`](https://jmw86069.github.io/jamba/reference/log2signed.md),
[`middle()`](https://jmw86069.github.io/jamba/reference/middle.md),
[`minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md),
[`newestFile()`](https://jmw86069.github.io/jamba/reference/newestFile.md),
[`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md),
[`reload_qmd_cache()`](https://jmw86069.github.io/jamba/reference/reload_qmd_cache.md),
[`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md),
[`renameColumn()`](https://jmw86069.github.io/jamba/reference/renameColumn.md),
[`rmInfinite()`](https://jmw86069.github.io/jamba/reference/rmInfinite.md),
[`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md),
[`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md),
[`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)

## Examples

``` r
testlist <- list(
   A=c(1, 4, 5, NA, 11),
   B=c("B", NA, "C", "Test"))
rmNAs(testlist)
#> $A
#> [1]  1  4  5 11
#> 
#> $B
#> [1] "B"    "C"    "Test"
#> 

testlist2 <- list(
   A=c(1, 4, 5, NA, 11, Inf),
   B=c(11, NA, 19, -Inf))
rmNAs(testlist2, naValue=-100, infiniteValue=1000)
#> $A
#> [1]    1    4    5 -100   11 1000
#> 
#> $B
#> [1]    11  -100    19 -1000
#> 
```
