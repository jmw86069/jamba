# remove NULL entries from list

remove NULL entries from list

## Usage

``` r
rmNULL(x, nullValue = NULL, ...)
```

## Arguments

- x:

  `list` or other object which may contain NULL.

- nullValue:

  `character` optional replacement value, default NULL, which causes the
  entry to be removed.

- ...:

  additional arguments are ignored.

## Value

list with NULL entries either removed, or replaced with nullValue. This
function is typically called so it removed list elements which are NULL,
resulting in a list that contains non-NULL entries. This function can
also be useful when NULL values should be changed to something else,
perhaps a character value "NULL" to be used as a label.

## Details

This function is a simple helper function to remove NULL from a list,
optionally replacing it with another value

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
[`rmNAs()`](https://jmw86069.github.io/jamba/reference/rmNAs.md),
[`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)

## Examples

``` r
x <- list(A=1:6, B=NULL, C=letters[11:16]);
rmNULL(x)
#> $A
#> [1] 1 2 3 4 5 6
#> 
#> $C
#> [1] "k" "l" "m" "n" "o" "p"
#> 
rmNULL(x, nullValue=NA)
#> $A
#> [1] 1 2 3 4 5 6
#> 
#> $B
#> [1] NA
#> 
#> $C
#> [1] "k" "l" "m" "n" "o" "p"
#> 
```
