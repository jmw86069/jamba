# Get aspect ratio for coordinates, plot, or device

Get aspect ratio for coordinates, plot, or device

## Usage

``` r
getPlotAspect(
  type = c("coords", "plot", "device"),
  parUsr = graphics::par("usr"),
  parPin = graphics::par("pin"),
  parDin = graphics::par("din"),
  ...
)
```

## Arguments

- type:

  `character` type of aspect ratio to calculate.

  "coords"

  :   calculates plot coordinate aspect ratio, which is helpful for
      creating proper circular shapes, for example, where the x-axis and
      y-axis ranges are very different. Note that this calculation does
      also correct for margin sizes.

  "plot"

  :   calculates plot aspect ratio, based upon the actual size of the
      plot, independent of the numeric coordinate range of the plot.
      This aspect ratio reflects the relative visual height and width of
      the plot area, ignoring margins.

  "device"

  :   calculates plot aspect ratio, based upon the complete graphical
      device, i.e. the full space including all panels, margins, and
      plot areas.

- parUsr, parPin, parDin:

  `numeric` values equivalent to their respective
  [`graphics::par()`](https://rdrr.io/r/graphics/par.html) output, from
  `graphics::par("usr")`, `graphics::par("pin")`, and
  `graphics::par("din")`. Values can be supplied directly, which among
  other things, prevents opening a graphical device if one is not
  already opened. Any call to
  [`graphics::par()`](https://rdrr.io/r/graphics/par.html) will
  otherwise cause a graphic device to be opened, which may not be
  desired on a headless R server.

- ...:

  additional parameters are ignored.

## Value

`numeric` plot aspect ratio for a plot device, of the requested type,
see the `type` argument.

## See also

Other jam plot functions:
[`adjustAxisLabelMargins()`](https://jmw86069.github.io/jamba/reference/adjustAxisLabelMargins.md),
[`coordPresets()`](https://jmw86069.github.io/jamba/reference/coordPresets.md),
[`decideMfrow()`](https://jmw86069.github.io/jamba/reference/decideMfrow.md),
[`drawLabels()`](https://jmw86069.github.io/jamba/reference/drawLabels.md),
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
withr::with_par(list("mfrow"=c(2, 4), "mar"=c(1, 1, 1, 1)), {
for (i in 1:8) {
   nullPlot(plotAreaTitle=paste("Plot", i), xlim=c(1,100), ylim=c(1,10),
      doMargins=FALSE);
   graphics::axis(1, las=2);
   graphics::axis(2, las=2);
}
# device aspect inside the 2x4 layout
getPlotAspect("plot");
})

#> [1] 0.4569939
# device aspect outside the 2x4 layout
getPlotAspect("plot");
#> [1] 1.124309
```
