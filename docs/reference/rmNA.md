# remove NA values

remove NA values

## Usage

``` r
rmNA(
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

  vector input

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

vector with NA entries either removed, or replaced with naValue, and
NULL entries either removed or replaced by nullValue.

## Details

This function removes NA values, by default shortening a vector as a
result, but optionally replacing NA and Infinite values with fixed
values.

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
[`rmNAs()`](https://jmw86069.github.io/jamba/reference/rmNAs.md),
[`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md),
[`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)

## Examples

``` r
# by default it removes NA, shortening the vector
rmNA(c(1, 5, 4, NA, 10, NA))
#> [1]  1  5  4 10

# convenient to replace NA with a fixed value
rmNA(c(1, 5, 4, NA, 10, NA), naValue=0)
#> [1]  1  5  4  0 10  0

m <- matrix(ncol=3, 1:9)
m[1, 2] <- NA;
rmNA(m, naValue=-1)
#>      [,1] [,2] [,3]
#> [1,]    1   -1    7
#> [2,]    2    5    8
#> [3,]    3    6    9

# by default NA and Inf is removed
rmNA(c(1, 5, 4, NA, 10, NA, Inf, -Inf))
#> [1]  1  5  4 10

# NA and Inf can be replaced, note Inf retains the sign
rmNA(c(1, 5, 4, NA, 10, NA, Inf, -Inf), naValue=0, infiniteValue=100)
#> [1]    1    5    4    0   10    0  100 -100
```
