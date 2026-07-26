# Create a blank plot with optional labels

Create a blank plot with optional labels for margins

## Usage

``` r
nullPlot(
  xaxt = "n",
  yaxt = "n",
  xlab = "",
  ylab = "",
  col = "transparent",
  xlim = c(1, 2),
  ylim = c(1, 2),
  las = graphics::par("las"),
  doBoxes = TRUE,
  doUsrBox = doBoxes,
  fill = "#FFFF9966",
  doAxes = FALSE,
  doMargins = TRUE,
  marginUnit = c("lines", "inches"),
  plotAreaTitle = "Plot Area",
  plotSrt = 0,
  plotNumPrefix = "",
  bty = "n",
  showMarginsOnly = FALSE,
  add = FALSE,
  ...
)
```

## Arguments

- xaxt:

  `character` value compatible with`options("xaxt")`

- yaxt:

  `character` value compatible with `options("xaxt")`

- xlab:

  `character` x-axis label

- ylab:

  `character` y-axis label

- col:

  `character` colors passed to
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html)

- xlim:

  `numeric` x-axis range

- ylim:

  `numeric` y-axis range

- las:

  `integer` value indicating whether axis labels should be parallel (1)
  or perpendicular (2) to the axis line.

- doBoxes:

  `logical` whether to draw annotated boxes around the plot and inner
  and outer margins.

- doUsrBox:

  `logical` whether to draw a colored bow indicating the exact plot
  space, using the function
  [`usrBox()`](https://jmw86069.github.io/jamba/reference/usrBox.md).

- fill:

  `character` R color used to fill the background of the plot as used by
  [`usrBox()`](https://jmw86069.github.io/jamba/reference/usrBox.md).

- doAxes:

  `logical` whether to draw default x- and y-axes.

- doMargins:

  `logical` whether to label margins, only active when doBoxes=TRUE.

- marginUnit:

  `character` indicating the units used for margin labels.

- plotAreaTitle:

  `character` label printed in the center of the plot area.

- plotSrt:

  numeric angle for the plotAreaTitle, which is good for labeling this
  plot with vertical text when displaying a plot panel inside a grid
  layout, where the plot is taller than it is wide.

- plotNumPrefix:

  `character` or integer label appended as suffix to margin labels,
  which is useful when annotating multiple plots in a grid layout, where
  labels are sometimes quite close together. This label is but a simple
  attempt to sidestep the real problem of fitting labels inside each
  visual component.

- bty:

  `character` passed
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html), default
  `"n"` suppresses the default box, which can then be optionally drawn
  based upon the `doBoxes` parameter.

- showMarginsOnly:

  `logical` whether to create a new plot or to annotate an existing
  active plot.

- add:

  `logical` whether to add to an existing active R plot, or create a new
  plot window.

- ...:

  additional arguments are ignored.

## Value

no output, this function is called for the byproduct of creating a blank
plot, optionally annotating the margins.

## Details

This function creates an empty plot space, using the current
[`graphics::par()`](https://rdrr.io/r/graphics/par.html) settings for
margins, text size, etc. By default it displays a box around the plot
window, and labels the margins and plot area for review. It can be
useful as a visual display of various base graphics settings, or to
create an empty plot window with pre-defined axis ranges. Lastly, one
can use this function to create a "blank" plot which uses a defined
background color, which can be a useful precursor to drawing an image
density which may not cover the whole plot space.

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
nullPlot()


nullPlot(doBoxes=FALSE)

```
