# log2 transformation with directionality

log2 transformation with directionality

## Usage

``` r
log2signed(x, offset = 1, base = 2, ...)
```

## Arguments

- x:

  `numeric` vector

- offset:

  `numeric` value added to the absolute values of `x` prior to applying
  the log transformation.

- base:

  `numeric` value indicating the logarithmic base, by default `2` in
  order to apply [`base::log2()`](https://rdrr.io/r/base/Log.html).

- ...:

  additional arguments are ignored.

## Value

numeric vector of log-transformed magnitudes.

## Details

This function applies a log2 transformation but maintains the sign of
the input data, allowing for log2 transformation of negative values.

The method applies an offset to the absolute value `abs(x)`, in order to
handle values between zero and 1, then applies log2 transformation, then
multiplies by the original sign from `sign(x)`.

The argument `offset` is used to adjust values, for example `offset=1`
will apply log2 transformation `log2(1 + x)`, except using the absolute
value of `x`. This method allows for positive and negative input data to
contain values between 0 and 1, and between -1 and 0.

This function could be described as applying a log2 transformation of
the "magnitude" of values in `x`, while maintaining the positive or
negative directionality.

If any `abs(x)` are less than `offset` this function will raise an
error.

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
log2signed(x);
#>   [1] -3.4594316 -3.4462562 -3.4329594 -3.4195389 -3.4059924 -3.3923174
#>   [7] -3.3785116 -3.3645724 -3.3504972 -3.3362834 -3.3219281 -3.3074285
#>  [13] -3.2927817 -3.2779847 -3.2630344 -3.2479275 -3.2326608 -3.2172307
#>  [19] -3.2016339 -3.1858665 -3.1699250 -3.1538053 -3.1375035 -3.1210154
#>  [25] -3.1043367 -3.0874628 -3.0703893 -3.0531113 -3.0356239 -3.0179219
#>  [31] -3.0000000 -2.9818527 -2.9634741 -2.9448584 -2.9259994 -2.9068906
#>  [37] -2.8875253 -2.8678965 -2.8479969 -2.8278190 -2.8073549 -2.7865964
#>  [43] -2.7655347 -2.7441611 -2.7224660 -2.7004397 -2.6780719 -2.6553518
#>  [49] -2.6322682 -2.6088092 -2.5849625 -2.5607150 -2.5360529 -2.5109619
#>  [55] -2.4854268 -2.4594316 -2.4329594 -2.4059924 -2.3785116 -2.3504972
#>  [61] -2.3219281 -2.2927817 -2.2630344 -2.2326608 -2.2016339 -2.1699250
#>  [67] -2.1375035 -2.1043367 -2.0703893 -2.0356239 -2.0000000 -1.9634741
#>  [73] -1.9259994 -1.8875253 -1.8479969 -1.8073549 -1.7655347 -1.7224660
#>  [79] -1.6780719 -1.6322682 -1.5849625 -1.5360529 -1.4854268 -1.4329594
#>  [85] -1.3785116 -1.3219281 -1.2630344 -1.2016339 -1.1375035 -1.0703893
#>  [91] -1.0000000 -0.9259994 -0.8479969 -0.7655347 -0.6780719 -0.5849625
#>  [97] -0.4854268 -0.3785116 -0.2630344 -0.1375035  0.0000000  0.1375035
#> [103]  0.2630344  0.3785116  0.4854268  0.5849625  0.6780719  0.7655347
#> [109]  0.8479969  0.9259994  1.0000000  1.0703893  1.1375035  1.2016339
#> [115]  1.2630344  1.3219281  1.3785116  1.4329594  1.4854268  1.5360529
#> [121]  1.5849625  1.6322682  1.6780719  1.7224660  1.7655347  1.8073549
#> [127]  1.8479969  1.8875253  1.9259994  1.9634741  2.0000000  2.0356239
#> [133]  2.0703893  2.1043367  2.1375035  2.1699250  2.2016339  2.2326608
#> [139]  2.2630344  2.2927817  2.3219281  2.3504972  2.3785116  2.4059924
#> [145]  2.4329594  2.4594316  2.4854268  2.5109619  2.5360529  2.5607150
#> [151]  2.5849625  2.6088092  2.6322682  2.6553518  2.6780719  2.7004397
#> [157]  2.7224660  2.7441611  2.7655347  2.7865964  2.8073549  2.8278190
#> [163]  2.8479969  2.8678965  2.8875253  2.9068906  2.9259994  2.9448584
#> [169]  2.9634741  2.9818527  3.0000000  3.0179219  3.0356239  3.0531113
#> [175]  3.0703893  3.0874628  3.1043367  3.1210154  3.1375035  3.1538053
#> [181]  3.1699250  3.1858665  3.2016339  3.2172307  3.2326608  3.2479275
#> [187]  3.2630344  3.2779847  3.2927817  3.3074285  3.3219281  3.3362834
#> [193]  3.3504972  3.3645724  3.3785116  3.3923174  3.4059924  3.4195389
#> [199]  3.4329594  3.4462562  3.4594316
plot(x=x, y=log2signed(x), xlab="x", ylab="log2signed(x)")

```
