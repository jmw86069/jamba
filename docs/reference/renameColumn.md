# Rename columns in a data.frame, matrix, tibble, or GRanges object

Rename columns in a data.frame, matrix, tibble, or GRanges object

## Usage

``` r
renameColumn(x, from, to, verbose = FALSE, ...)
```

## Arguments

- x:

  `data.frame`, `matrix`, `tbl`, or `GRanges` equivalent object. It will
  work on any object for which
  [`colnames()`](https://rdrr.io/r/base/colnames.html) is defined.

- from:

  `character` vector of colnames expected to be in `x`. Any values that
  do not match `colnames(x)` are ignored.

- to:

  `character` vector with `length(to) == length(from)` corresponding to
  the target name for any colnames that match `from`.

- verbose:

  `logical` indicating whether to print verbose output.

- ...:

  Additional arguments are ignored.

## Value

`data.frame` or object equivalent to the input `x`, with columns `from`
renamed to values in `to`. For genomic ranges objects such as `GRanges`
and `IRanges`, the colnames are updated in `S4Vectors::values(x)`.

## Details

This function is intended to rename one or more columns in a
`data.frame`, `matrix`, tibble, or `GRanges` related object. It will
gracefully ignore columns which do not match, in order to make it
possible to call the function again without problem.

This function will also recognize input objects `GRanges`, `ucscData`,
and `IRanges`, which store annotation in `DataFrame` accessible via
[`S4Vectors::values()`](https://rdrr.io/pkg/S4Vectors/man/Vector-class.html).
Note the `IRanges` package is required, for its generic function
`values()`.

The values supplied in `to` and `from` are converted from `factor` to
`character` to avoid coersion by R to `integer`, which was noted in
output prior to jamba version `0.0.72.900`.

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
[`rmInfinite()`](https://jmw86069.github.io/jamba/reference/rmInfinite.md),
[`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md),
[`rmNAs()`](https://jmw86069.github.io/jamba/reference/rmNAs.md),
[`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md),
[`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)

## Examples

``` r
df <- data.frame(A=1:5, B=6:10, C=11:15);
df;
#>   A  B  C
#> 1 1  6 11
#> 2 2  7 12
#> 3 3  8 13
#> 4 4  9 14
#> 5 5 10 15
df2 <- renameColumn(df,
   from=c("A","C"),
   to=c("a_new", "c_new"));
df2;
#>   a_new  B c_new
#> 1     1  6    11
#> 2     2  7    12
#> 3     3  8    13
#> 4     4  9    14
#> 5     5 10    15
df3 <- renameColumn(df2,
   from=c("A","C","B"),
   to=c("a_new", "c_new","b_new"));
df3;
#>   a_new b_new c_new
#> 1     1     6    11
#> 2     2     7    12
#> 3     3     8    13
#> 4     4     9    14
#> 5     5    10    15
```
