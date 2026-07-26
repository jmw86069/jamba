# Calculate scatter plot point density

Calculate scatter plot point density

## Usage

``` r
jamCalcDensity(x, nbin, bandwidth = NULL, range.x)
```

## Arguments

- x:

  `numeric` matrix with two columns representing x,y coordinates.

- nbin:

  `integer` number of bins to subdivide the scatterplot, expanded to
  length 2 to accommodate x and y axis bins.

- bandwidth:

  `numeric` or 'NULL' representing the bandwidth used for point density
  determination.

- range.x:

  `numeric` vector length 2 representing the range of values to consider
  for point density.

## Value

`list` with elements used internally by
[`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md),
with: x1, x2, fhat, bandwidth.

## Details

This function is called internally by
[`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md),
and is an equivalent replacement for `grDevices` non-exported function
.smoothScatterCalcDensity(), understandably a requirement by CRAN. A
package should not rely on another package hidden function.

## See also

Other jam internal functions:
[`handleArgsText()`](https://jmw86069.github.io/jamba/reference/handleArgsText.md),
[`make_html_styles()`](https://jmw86069.github.io/jamba/reference/make_html_styles.md),
[`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md),
[`smoothScatterJam()`](https://jmw86069.github.io/jamba/reference/smoothScatterJam.md)

## Examples

``` r
sdim(jamCalcDensity(cbind(x=rnorm(1000) + 4, y=rnorm(1000) + 4), nbin=30))
#>           rows cols   class class_v2
#> x1          30      numeric     <NA>
#> x2          30      numeric     <NA>
#> fhat        30   30  matrix    array
#> bandwidth    1    2  matrix    array
```
