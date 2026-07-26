# exponentiate log2 values with directionality

exponentiate log2 values with directionality

## Usage

``` r
exp2signed(x, offset = 1, base = 2, ...)
```

## Arguments

- x:

  `numeric` vector

- offset:

  `numeric` subtracted from exponentiated values prior to multiplying by
  the `sign(x)`.

- base:

  `numeric` value indicating the logarithmic base used. For example
  `base=2` indicates values were transformed using
  [`log2()`](https://rdrr.io/r/base/Log.html).

- ...:

  additional arguments are ignored.

## Value

numeric vector of exponentiated values.

## Details

This function is the reciprocal to
[`log2signed()`](https://jmw86069.github.io/jamba/reference/log2signed.md).

It \#' exponentiates the absolute values of `x`, then subtracts the
`offset`, then multiplies results by the `sign(x)`.

The `offset` is typically used to maintain directionality of values
during log transformation by requiring all absolute values to be `1` or
larger, thus by default `offset=1`.

## See also

Other jam practical functions:
[`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md),
[`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md),
[`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md),
[`check_pkg_installed()`](https://jmw86069.github.io/jamba/reference/check_pkg_installed.md),
[`colNum2excelName()`](https://jmw86069.github.io/jamba/reference/colNum2excelName.md),
[`color_dither()`](https://jmw86069.github.io/jamba/reference/color_dither.md),
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
[`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md),
[`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)

## Examples

``` r
x <- c(-100:100)/10;
z <- log2signed(x);
#plot(x=x, y=z, xlab="x", ylab="log2signed(x)")
plot(x=x, y=exp2signed(z), xlab="x", ylab="exp2signed(log2signed(x))")

plot(x=z, y=exp2signed(z), xlab="log2signed(x)", ylab="exp2signed(log2signed(x))")

```
