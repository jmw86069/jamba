# Return the middle portion of data similar to head and tail

Return the middle portion of data similar to head and tail

## Usage

``` r
middle(x, n = 10, evenly = TRUE, ...)
```

## Arguments

- x:

  input data that can be subset

- n:

  `numeric` number of entries to return

- evenly:

  `logical` indicating whether to return evenly spaced entries along the
  full length of `x`. When `evenly=FALSE` only the middle `n` entries
  are returned.

- ...:

  additional arguments are ignored.

## Value

an object of class equivalent to `x`.

## Details

This function is very simple, and is intended to mimic
[`head()`](https://rdrr.io/r/utils/head.html) and
[`tail()`](https://rdrr.io/r/utils/head.html) to inspect data without
looking at every value

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
[`minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md),
[`newestFile()`](https://jmw86069.github.io/jamba/reference/newestFile.md),
[`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md),
[`reload_qmd_cache()`](https://jmw86069.github.io/jamba/reference/reload_qmd_cache.md),
[`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md),
[`renameColumn()`](https://jmw86069.github.io/jamba/reference/renameColumn.md),
[`rmInfinite()`](https://jmw86069.github.io/jamba/reference/rmInfinite.md),
[`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md),
[`rmNAs()`](https://jmw86069.github.io/jamba/reference/rmNAs.md),
[`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md),
[`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)

## Examples

``` r
x <- 1:101;
middle(x);
#>  [1]   1  12  23  34  45  57  68  79  90 101
middle(x, evenly=TRUE)
#>  [1]   1  12  23  34  45  57  68  79  90 101

xdf <- data.frame(n=1:101,
   excel_colname=jamba::colNum2excelName(1:101));
middle(xdf)
#>       n excel_colname
#> 1     1             A
#> 12   12             L
#> 23   23             W
#> 34   34            AH
#> 45   45            AS
#> 57   57            BE
#> 68   68            BP
#> 79   79            CA
#> 90   90            CL
#> 101 101            CW
middle(xdf, evenly=TRUE)
#>       n excel_colname
#> 1     1             A
#> 12   12             L
#> 23   23             W
#> 34   34            AH
#> 45   45            AS
#> 57   57            BE
#> 68   68            BP
#> 79   79            CA
#> 90   90            CL
#> 101 101            CW
```
