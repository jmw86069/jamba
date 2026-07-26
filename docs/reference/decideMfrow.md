# Decide plot panel rows, columns for graphics::par(mfrow)

Decide plot panel rows, columns for graphics::par(mfrow)

## Usage

``` r
decideMfrow(
  n,
  method = c("aspect", "wide", "tall"),
  doTest = FALSE,
  xyratio = 1,
  trimExtra = TRUE,
  ...
)
```

## Arguments

- n:

  `integer` number of plot panels

- method:

  `character` string indicating the type of layout to favor.

  "aspect"

  :   uses the device size and aspect ratio of the plot to try to
      maintain roughly square plot panels.

  "wide"

  :   tries to keep the columns and rows similar, erring on the side of
      more columns than rows.

  "tall"

  :   tries to keep the columns and rows similar, erring on the side of
      more rows than columns.

- doTest:

  `logical` whether to provide a visual test. Note that `n` is required
  as the number of plot panels requested.

- xyratio:

  `numeric` default 1, with the desired target x-to-y ratio. For
  example, to have plots slightly wider (x width) than tall (y height),
  use `xyratio=1.3`. The observed device aspect ratio is divided by
  `xyratio` to determine the target aspect ratio of plot panels.

- trimExtra:

  `logical` default TRUE, whether to trim blank rows or columns in the
  expected layout when it would be entirely blank. For example, `n=4`
  may produce `c(3, 2)` output to meet the desired aspect ratio, however
  with `trimExtra=TRUE` it would be reduced to `c(2, 2)` to minimize
  unused whitespace.

- ...:

  additional parameters are ignored.

## Value

`numeric` vector length=2, with the recommended number of plot rows and
columns, respectively. It is intended to be used directly in this form:
`graphics::par("mfrow"=decideMfrow(n=5))`

## Details

This function returns the recommended rows and columns of panels to be
used in `graphics::par("mfrow")` with R base plotting. It attempts to
use the device size and plot aspect ratio to keep panels roughly square.
For example, a short-wide device would have more columns of panels than
rows; a tall-thin device would have more rows than columns.

The `doTest=TRUE` argument will create `n` number of panels with the
recommended layout, as a visual example.

Note this function calls
[`getPlotAspect()`](https://jmw86069.github.io/jamba/reference/getPlotAspect.md),
therefore if no plot device is currently open, the call to
[`graphics::par()`](https://rdrr.io/r/graphics/par.html) will open a new
graphics device.

## See also

Other jam plot functions:
[`adjustAxisLabelMargins()`](https://jmw86069.github.io/jamba/reference/adjustAxisLabelMargins.md),
[`coordPresets()`](https://jmw86069.github.io/jamba/reference/coordPresets.md),
[`drawLabels()`](https://jmw86069.github.io/jamba/reference/drawLabels.md),
[`getPlotAspect()`](https://jmw86069.github.io/jamba/reference/getPlotAspect.md),
[`groupedAxis()`](https://jmw86069.github.io/jamba/reference/groupedAxis.md),
[`imageByColors()`](https://jmw86069.github.io/jamba/reference/imageByColors.md),
[`imageDefault()`](https://jmw86069.github.io/jamba/reference/imageDefault.md),
[`minorLogTicksAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md),
[`nullPlot()`](https://jmw86069.github.io/jamba/reference/nullPlot.md),
[`plotPolygonDensity()`](https://jmw86069.github.io/jamba/reference/plotPolygonDensity.md),
[`plotRidges()`](https://jmw86069.github.io/jamba/reference/plotRidges.md),
[`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md),
[`shadowText()`](https://jmw86069.github.io/jamba/reference/shadowText.md),
[`shadowText_options()`](https://jmw86069.github.io/jamba/reference/shadowText_options.md),
[`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md),
[`sqrtAxis()`](https://jmw86069.github.io/jamba/reference/sqrtAxis.md),
[`usrBox()`](https://jmw86069.github.io/jamba/reference/usrBox.md)

## Examples

``` r
# display a test visualization showing 6 panels
withr::with_par(list("mar"=c(2, 2, 2, 2)), {
decideMfrow(n=6, doTest=TRUE);
})

#> [1] 3 2

# use a custom target xyratio of plot panels
withr::with_par(list("mar"=c(2, 2, 2, 2)), {
decideMfrow(n=3, xyratio=3, doTest=TRUE);
})

#> [1] 3 1

# a manual demonstration creating 6 panels
n <- 6;
withr::with_par(list(
   "mar"=c(2, 2, 2, 2),
   "mfrow"=decideMfrow(n)), {
for(i in seq_len(n)){
   nullPlot(plotAreaTitle=paste("Plot", i));
}
})

```
