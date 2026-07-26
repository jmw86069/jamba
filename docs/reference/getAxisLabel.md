# Get axis label for minorLogTicks

Get axis label for minorLogTicks

## Usage

``` r
getAxisLabel(
  i,
  asValues,
  logAxisType = c("normal", "flip", "pvalue"),
  logBase,
  base_limit = 2,
  offset = 0,
  symmetricZero = (offset > 0),
  ...
)
```

## Arguments

- i:

  `numeric` axis value

- asValues:

  `logical` indicating whether the value should be evaluated.

- logAxisType:

  `character` string with the type of axis values:

  - `"normal"`: axis values as-is.

  - `"flip"`: inverted axis values, for example where negative values
    should be displayed as negative log-transformed values.

  - `"pvalue"`: for values transformed as `-log10(pvalue)`

- logBase:

  `numeric` logarithmic base

- base_limit:

  `numeric` value indicating the minimum value that should be written as
  an exponential.

- offset:

  `numeric` value of offset used for log transformation.

- symmetricZero:

  `logical` indicating whether negative values should be displayed as
  negative log-transformed values.

- ...:

  additional arguments are ignored.

## Value

`character` or `expression` axis label as appropriate.

## Details

This function is intended to be called internally by
[`jamba::minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md).

## See also

Other jam practical functions:
[`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md),
[`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md),
[`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md),
[`check_pkg_installed()`](https://jmw86069.github.io/jamba/reference/check_pkg_installed.md),
[`colNum2excelName()`](https://jmw86069.github.io/jamba/reference/colNum2excelName.md),
[`color_dither()`](https://jmw86069.github.io/jamba/reference/color_dither.md),
[`exp2signed()`](https://jmw86069.github.io/jamba/reference/exp2signed.md),
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
[`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md),
[`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)

## Examples

``` r
x <- log10(c(1, 2, 5, 10, 20, 50, 100, 200, 500))
getAxisLabel(x, asValues=TRUE, logBase=10)
#> [1]   1   2   5  10  20  50 100 200 500

x1exp <- c(1, 2, 3, 4, 5)
plot(1:6, main="exponential values")
for (i in seq_along(x1exp)) {
   text(x=i, y=i + 0.2,
      getAxisLabel(x1exp[i], asValues=FALSE, logBase=10))
}


x1exp <- c(-3:3)
plot(-3:3, main="log2 fold change values")
for (i in seq_along(x1exp)) {
   text(x=i, y=i + 0.3 - 4,
      getAxisLabel(x1exp[i],
         logAxisType="flip",
         asValues=TRUE, logBase=2))
}


x1exp <- c(1, 2, 3, 4, 5)
plot(1:6, main="P-value style")
for (i in seq_along(x1exp)) {
   text(x=i, y=i + 0.2,
      getAxisLabel(x1exp[i],
      logAxisType="pvalue", asValues=FALSE, logBase=10))
}

```
