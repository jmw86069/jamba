# Draw colored box indicating R plot space

Draw colored box indicating the active R plot space

## Usage

``` r
usrBox(
  fill = "#FFFF9966",
  label = NULL,
  parUsr = graphics::par("usr"),
  debug = FALSE,
  ...
)
```

## Arguments

- fill:

  `character` R color used to fill the background of the plot

- label:

  `character` text optionally used to label the center of the plot
  space, default 'NULL'

- parUsr:

  `numeric` vector length 4, indicating the R plot space, consistent
  with `graphics::par("usr")`. It can thus be used to define a different
  area, though using the [`rect`](https://rdrr.io/r/graphics/rect.html)
  function directly seems more appropriate.

- debug:

  `logical` whether to print the parUsr value being used.

- ...:

  additional arguments are ignored.

## Value

no output, this function is called for the byproduct of adding a box in
the usr plot space of an R graphics device.

## Details

This function simply draws a box indicating the active plot space, and
by default it shades the box light yellow with transparency. It can be
useful to indicate the active plot area while allowing pre-drawn plot
elements to be shown, or can be useful precursor to provide a colored
background for the plot.

The plot space is defined using 'graphics::par("usr")' and therefore
requires an active R device is already opened.

## See also

Other jam plot functions:
[`adjustAxisLabelMargins()`](https://jmw86069.github.io/jamba/reference/adjustAxisLabelMargins.md),
[`coordPresets()`](https://jmw86069.github.io/jamba/reference/coordPresets.md),
[`decideMfrow()`](https://jmw86069.github.io/jamba/reference/decideMfrow.md),
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
[`sqrtAxis()`](https://jmw86069.github.io/jamba/reference/sqrtAxis.md)

## Examples

``` r
# usrBox requires that a plot device is already open
nullPlot(doBoxes=FALSE);
usrBox();

```
