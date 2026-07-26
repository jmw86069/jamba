# Plot distribution and histogram overlay

Plot distribution and histogram overlay

## Usage

``` r
plotPolygonDensity(
  x,
  doHistogram = TRUE,
  doPolygon = TRUE,
  col = NULL,
  barCol = "#00337799",
  polyCol = "#00449977",
  polyBorder = makeColorDarker(polyCol),
  histBorder = makeColorDarker(barCol, darkFactor = 1.5),
  colAlphas = c(0.8, 0.6, 0.9),
  darkFactors = c(-1.3, 1, 3),
  lwd = 2,
  las = 2,
  u5.bias = 0,
  pretty.n = 10,
  bw = NULL,
  breaks = 100,
  width = NULL,
  densityBreaksFactor = 3,
  axisFunc = graphics::axis,
  bty = "l",
  cex.axis = 1.5,
  doPar = TRUE,
  heightFactor = 0.95,
  weightFactor = NULL,
  main = "Histogram distribution",
  xaxs = "i",
  yaxs = "i",
  xaxt = "s",
  yaxt = "s",
  xlab = "",
  ylab = "",
  log = NULL,
  xScale = c("default", "log10", "sqrt"),
  usePanels = TRUE,
  useOnePanel = FALSE,
  ablineV = NULL,
  ablineH = NULL,
  ablineVcol = "#44444499",
  ablineHcol = "#44444499",
  ablineVlty = "solid",
  ablineHlty = "solid",
  removeNA = TRUE,
  add = FALSE,
  ylimQuantile = 0.99,
  ylim = NULL,
  xlim = NULL,
  highlightPoints = NULL,
  highlightCol = "gold",
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  `numeric` vector, or `numeric` matrix. When a matrix is provided, each
  column in the matrix is used as its own data source.

- doHistogram:

  `logical` indicating whether to plot histogram bars.

- doPolygon:

  `logical` indicating whether to plot the density polygon.

- col:

  `character` color, or when `x` is supplied as a matrix, a vector of
  colors is applied to across plot panels. Note that `col` will override
  all colors defined for `barCol`, `polyCol`, `histBorder`,
  `polyBorder`.

- barCol, polyCol, polyBorder, histBorder:

  `character` colors used when `col` is not supplied. They define colors
  for the histogram bars, polygon fill, polygon border, and histogram
  bar border, respectively.

- colAlphas:

  `numeric` vector with length 3, indicating the alpha transparency to
  use for histogram bar fill, polygon density fill, and border color,
  respectively. Alpha transparency should be scaled between 0 (fully
  transparent) and 1 (fully opaque). These alpha transparency values are
  applied to each color in `col` when `col` is defined.

- darkFactors:

  `numeric` used to adjust colors when `col` is defined. Values are
  applied to histogram bar fill, polygon density fill, and border color,
  respectively, by calling
  [`makeColorDarker()`](https://jmw86069.github.io/jamba/reference/makeColorDarker.md).

- lwd:

  `numeric` line width.

- las:

  `integer` used to define axis label orientation.

- u5.bias, pretty.n:

  `numeric` arguments passed to to
  [`base::pretty()`](https://rdrr.io/r/base/pretty.html) to define
  pretty axis label positions.

- bw:

  `character` string of the bandwidth name to use in the density
  calculation, passed to
  [`jamba::breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md).
  By default [`stats::density()`](https://rdrr.io/r/stats/density.html)
  calls a very smooth density kernel, which obscures finer details, so
  the default in
  [`jamba::breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md)
  uses a more detailed kernel.

- breaks:

  `numeric` breaks sent to `hist` to define the number of histogram
  bars. It can be in the form of a single `integer` number of
  equidistant breaks, or a `numeric` vector with specific break
  positions, but remember to include a starting value lower the the
  lowest value in `x`, and an ending value higher than the highest value
  in `x`. Passed to
  [`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md).

- width:

  `numeric` passed to
  [`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md).

- densityBreaksFactor:

  `numeric` scaling factor to control the level of detail in the
  density, passed to
  [`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md).

- axisFunc:

  `function` optionally used in place of
  [`graphics::axis()`](https://rdrr.io/r/graphics/axis.html) to define
  axis labels.

- bty:

  `character` string used to define the plot box shape, see
  [`graphics::box()`](https://rdrr.io/r/graphics/box.html).

- cex.axis:

  `numeric` scalar to adjust axis label font size.

- doPar:

  `logical` indicating whether to apply
  [`graphics::par()`](https://rdrr.io/r/graphics/par.html), specifically
  when `x` is supplied as a multi-column matrix. When `doPar=FALSE`, no
  panels nor margin adjustments are made at all.

- heightFactor:

  `numeric` value indicating the height of the y-axis plot scale to use
  when scaling the histogram and polygon density within each plot panel.

- weightFactor:

  `numeric` passed to
  [`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md).

- main:

  `character` title to display above the plot, used only when `x` is
  supplied as a single `numeric` vector. Otherwise each plot title uses
  the relevant `colnames(x)` value.

- xaxs, yaxs, xaxt, yaxt:

  `character` string indicating the type of x-axis and y-axis to render,
  see [`graphics::par()`](https://rdrr.io/r/graphics/par.html).

- xlab, ylab:

  `character` labels for x-axis and y-axis, respectively.

- log:

  `character` vector, optionally containing `"x"` and/or `"y"` to to
  indicate which axes are log-transformed. If `"x" %in% log` then it
  sets `xScale="log10"`, both methods are equivalent in defining the
  log-transformation of the x-axis.

- xScale:

  `character` string to define the x-axis transformation:

  - `"default"` applies no transform;

  - `"log10"` applies a log10 transform, specifically `log10(x + 1)`

  - `"sqrt"` applies a sqrt transform.

- usePanels:

  `logical` indicating whether to separate the density plots into panels
  when `x` contains multiple columns. When `useOnePanel=FALSE` the
  panels will be defined so that all columns will fit on one page.

- useOnePanel:

  `logical` indicating whether to define multiple panels on one page.
  Therefore `useOnePanel=TRUE` will create multiple pages with one panel
  on each page, which may work well for output in multi-page 'PDF'
  files.

- ablineV, ablineH:

  `numeric` vector representing abline vertical and horizontal
  positions, respectively. These values are mostly helpful in
  multi-panel plots, to draw consistent reference lines on each panel.

- ablineVcol, ablineHcol, :

  default"#44444499", with the abline color, used when `ablineV` or
  `ablineH` are supplied, respectively.

- ablineVlty, ablineHlty:

  `numeric` or `character` indicating the line type to use for `ablineV`
  and `ablineH`, respectively.

- removeNA:

  `logical` indicating whether to remove NA values prior to running
  histogram and density calculations. Presence of NA values generally
  causes both functions to fail.

- add:

  `logical` indicating whether to add the plot to an existing
  visualization.

- ylimQuantile:

  `numeric` value between 0 and 1, indicating the quantile value of the
  density `y` values to use for the ylim. This threshold is only applied
  when `ylim` is NULL.

- ylim, xlim:

  `numeric` y-axis and x-axis ranges, respectively. When either is
  'NULL', the axis range is determined independently for each plot
  panel. Either value can be supplied as a `list` to control the numeric
  range for each individual plot, relevant only when `x` is supplied as
  a multi-column matrix.

- highlightPoints:

  `character` vector of optional rownames, or `integer` values with row
  indices, for rows to be highlighted. When `x` is supplied as a
  `matrix`, `highlightPoints` can be supplied as a `list` of vectors,
  referring to each column in `x`. When rows are highlighted, the plot
  is drawn with all points, then the highlighted points are drawn again
  over the histogram bars, and polygon density, as relevant.

- highlightCol:

  `character` vector of colors to use to fill the histogram when
  `highlightPoints` is supplied. Multiple values are recycled one per
  column in `x`, if `x` is supplied as a multi-column matrix.

- verbose:

  `logical` indicating whether to print verbose output.

- ...:

  additional arguments are passed to relevant internal functions.

## Value

invisible `list` with density and histogram data output, however this
function is called for the by-product of its plot output.

## Details

This function is a wrapper around
[`graphics::hist()`](https://rdrr.io/r/graphics/hist.html) and
[`stats::density()`](https://rdrr.io/r/stats/density.html), with enough
customization to cover most of the situations that need customization.

For example `log="x"` will automatically log-transform the x-axis,
keeping the histogram bars uniformly sized. Alternatively,
`xScale="sqrt"` will square root transform the data, and transform the
x-axis while keeping the numeric values constant.

It also scales the density profile height to be similar to the histogram
bar height, using the 99th quantile of the y-axis value, which helps
prevent outlier peaks from dominating the y-axis range, thus obscuring
interesting smaller features.

If supplied with a data matrix, this function will create a layout with
`ncol(x)` panels, and plot the distribution of each column in its own
panel, using categorical colors from
[`rainbow2()`](https://jmw86069.github.io/jamba/reference/rainbow2.md).

For a similar style using ggplot2, see
[`plotRidges()`](https://jmw86069.github.io/jamba/reference/plotRidges.md),
which displays only the density profile for each sample, but in a much
more scalable format for larger numbers of columns.

By default NA values are ignored, and the distributions represent non-NA
values.

Colors can be controlled using the parameter `col`, but can be
specifically defined for bars with `barCol` and the polygon with
`polyCol`.

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
[`plotRidges()`](https://jmw86069.github.io/jamba/reference/plotRidges.md),
[`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md),
[`shadowText()`](https://jmw86069.github.io/jamba/reference/shadowText.md),
[`shadowText_options()`](https://jmw86069.github.io/jamba/reference/shadowText_options.md),
[`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md),
[`sqrtAxis()`](https://jmw86069.github.io/jamba/reference/sqrtAxis.md),
[`usrBox()`](https://jmw86069.github.io/jamba/reference/usrBox.md)

## Examples

``` r
# basic density plot
set.seed(123);
x <- stats::rnorm(2000);
plotPolygonDensity(x, main="basic polygon density plot");


# fewer breaks
plotPolygonDensity(x,
   breaks=20,
   main="breaks=20");


# log-scaled x-axis
plotPolygonDensity(10^(3+stats::rnorm(2000)), log="x",
   breaks=50,
   main="log-scaled x-axis");


# highlighted points
set.seed(123);
plotPolygonDensity(x,
   highlightPoints=sample(which(abs(x) > 1), size=200),
   breaks=40,
   main="breaks=40");


# hide axis labels
set.seed(123);
plotPolygonDensity(x,
   highlightPoints=sample(which(abs(x) > 1), size=200),
   breaks=40,
   xaxt="n",
   yaxt="n",
   main="breaks=40");


# multiple columns
set.seed(123);
xm <- do.call(cbind, lapply(1:4, function(i){stats::rnorm(2000)}))
plotPolygonDensity(xm, breaks=20)

```
