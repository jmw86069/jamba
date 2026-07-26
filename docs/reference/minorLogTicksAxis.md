# Display major and minor tick marks for log-scale axis

Display major and minor tick marks for log-scale axis, with optional
offset for proper labeling of `log2(1+x)` with numeric offset.

Log fold axis

## Usage

``` r
minorLogTicksAxis(
  side = NULL,
  lims = NULL,
  logBase = 2,
  displayBase = 10,
  offset = 0,
  symmetricZero = (offset > 0),
  majorCex = 1,
  minorCex = 0.65,
  doMajor = TRUE,
  doMinor = TRUE,
  doLabels = TRUE,
  doMinorLabels = NULL,
  asValues = TRUE,
  logAxisType = c("normal", "flip", "pvalue"),
  padj = NULL,
  doFormat = TRUE,
  big.mark = ",",
  scipen = 10,
  minorWhich = c(2, 5),
  logStep = 1,
  cex = 1,
  las = 2,
  col = "black",
  col.ticks = col,
  minorLogTicksData = NULL,
  verbose = FALSE,
  ...
)

logFoldAxis(
  side = NULL,
  lims = NULL,
  logBase = 2,
  displayBase = 2,
  offset = 0,
  symmetricZero = TRUE,
  asValues = TRUE,
  minorWhich = NULL,
  doMinor = TRUE,
  doMinorLabels = NULL,
  scipen = 1,
  ...
)

pvalueAxis(
  side = 2,
  lims = NULL,
  displayBase = 10,
  logBase = 10,
  logAxisType = "pvalue",
  asValues = FALSE,
  doMinor = FALSE,
  doMinorLabels = FALSE,
  scipen = 1,
  ...
)
```

## Arguments

- side:

  `integer` indicating the axis side, 1=bottom, 2=left, 3=top, 4=right.

- lims:

  NULL or `numeric` range for which the axis tick marks will be
  determined. If NULL then the corresponding `graphics::par("usr")` will
  be used.

- logBase:

  `numeric` value indicating the log base units, which will be used
  similar to how `base` is used in `log(x, base)`.

- displayBase:

  `numeric` value indicating the log base units to use when determining
  the numeric label position. For example, data may be log2 scaled, and
  yet it is visually intuitive to show log transformed axis units in
  base 10 units. See examples.

- offset:

  `numeric` offset used in transforming the numeric data displayed on
  this axis. For example, a common technique is to transform data using
  `log2(1+x)` which adds `1` to values prior to the log2 transformation.
  In this case, `offset=1`, which ensures the axis labels exactly match
  the initial numeric value prior to the log2 transform.

- symmetricZero:

  `logical` indicating whether numeric values are symmetric around zero.
  For example, log fold changes should use `symmetricZero=TRUE` which
  ensures a log2 value of `-2` is labeled `-4` to indicate a negative
  four fold change. If `symmetricZero=FALSE` a log2 value of `-2` would
  be labeled `0.0625`.

- majorCex, minorCex:

  `numeric` base text size factors, relative to cex=1 for default text
  size. These factors are applied in addition to existing
  `graphics::par("cex")` values, preserving any global text size defined
  there.

- doMajor, doMinor, doLabels:

  `logical`, default TRUE, whether to display each type of tick and
  label.

  - `doMajor` display major ticks, at `displayBase` positions

  - `doMinor` display minor ticks at intermediate positions

  - `doLabels` display any labels

  - `doMinorLabels` display minor labels

- doMinorLabels:

  `logical` whether to display numeric labels for minor ticks. Default
  NULL will show labels with fewer then 18 minor labels in range.

- asValues:

  `logical`, default TRUE, whether to print the exponentiated value,
  otherwise FALSE will print the log value.

- logAxisType:

  `character` string with the type of axis values:

  - `"normal"`: axis values as-is.

  - `"flip"`: inverted axis values, for example where negative values
    should be displayed as negative log-transformed values.

  - `"pvalue"`: for values transformed as `-log10(pvalue)`

- padj:

  `numeric` vector length 2, which is used to position axis labels for
  the minor and major labels, respectively. For example, `padj=c(0,1)`
  will position minor labels just to the left of the tick marks, and
  major labels just to the right of tick marks. This example is helpful
  when minor labels bunch up on the right side of each section.

- doFormat:

  `logical` indicating whether to apply
  [`base::format()`](https://rdrr.io/r/base/format.html) to format
  numeric labels.

- big.mark, scipen:

  arguments passed to
  [`base::format()`](https://rdrr.io/r/base/format.html) when
  `doFormat=TRUE`.

- minorWhich:

  `integer` vector indicating which of the minor tick marks should be
  labeled. Labels are generally numbered from `2` to `displayBase-1`. So
  by default, log 10 units would add minor tick marks and labels to the
  `c(2,5)` position. For log2 units only, the second label is defined at
  1.5, which shows minor labels at `c(3, 6, 12)`, which are
  `1.5 * c(2, 4, 8)`.

- logStep:

  `integer` the number of log units per "step", typically `1`.

- cex, col, col.ticks, las:

  parameters used for axis label size, axis label colors, axis tick mark
  colors, and label text orientation, respectively.

- minorLogTicksData:

  `list` object created by running
  [`jamba::minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md),
  which allows inspecting and modifying the content for custom control.

- verbose:

  `logical` indicating whether to print verbose output.

- ...:

  Additional arguments are ignored.

## Value

`list` with vectors:

- `majorLabels`: `character` vector of major axis labels

- `majorTicks`: `numeric` vector of major axis tick positions

- `minorLabels`: `character` vector of minor axis labels

- `minorTicks`: `numeric` vector of minor axis tick positions

- `allLabelsDF`: `data.frame` containing all axis tick positions and
  corresponding labels.

## Details

This function displays log units on the axis of an existing base R plot.
It calls
[`jamba::minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md)
which calculates appropriate tick and label positions.

Note: This function assumes the axis values have already been
log-transformed. Make sure to adjust the `offset` to reflect the method
of log-transformation, for example:

- `log2(1+x)` would require `logBase=2` and `offset=1` in order to
  represent values properly at or near zero.

- `log(0.5+x)` would require `logBase=exp(1)` and `offset=0.5`.

- `log10(x)` would require `logBase=10` and `offset=0`.

The defaults `logBase=2` and `displayBase=10` assume data has been
log2-transformed, and displays tick marks using the common base of 10.
To display tick marks at two-fold intervals, use `displayBase=2`.

This function was motivated in order to label log-transformed data
properly in some special cases, like using `log2(1+x)` where the
resulting values are shifted "off by one" using standard log-scaled axis
tick marks and labels.

For log fold changes, set `symmetricZero=TRUE`, which will create
negative log scaled fold change values as needed for negative values.
For example, this option would label a `logBase=2` value of `-2` as `-4`
and not as `0.25`.

Note that by default, whenever `offset > 0` the argument
`symmetricZero=TRUE` is also defined, since a negative value in that
scenario has little meaning. This behavior can be turned off by setting
`symmetricZero=FALSE`.

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
plotPolygonDensity(0:100, breaks=100);


plotPolygonDensity(0:100, breaks=50, log="x",
   main="plotPolygonDensity() uses minorLogTicksAxis()",
   xlab="x (log-scaled)");


plotPolygonDensity(log2(1+0:100), breaks=50,
   main="manually called minorLogTicksAxis(logBase=2)",
   xaxt="n",
   xlab="x (log-scaled)");
minorLogTicksAxis(1, offset=1, logBase=2);


plotPolygonDensity(log10(1+0:100), breaks=50,
   main="manually called minorLogTicksAxis(logBase=10)",
   xaxt="n",
   xlab="x (log-scaled)");
minorLogTicksAxis(1, offset=1, logBase=10);


# example with log fold axes
k <- c(-5:5)
plot(x=k, y=k, xaxt="n", yaxt="n",
   xlab="log2 base, displaying tick marks with log10 intervals",
   ylab="log2 base, displaying tick marks with log2 intervals")
axis(3, las=2)
axis(4, las=2)
lfax <- logFoldAxis(side=1, logBase=2, displayBase=2)
lfay <- logFoldAxis(side=2, logBase=2, displayBase=10)
# optionally add x-axis ablines
abline(v=lfax$allTicks, lty="dotted", col="grey88")
abline(v=lfax$majorTicks, lty="dashed", col="grey82")
# optionally add y-axis ablines
abline(h=lfay$allTicks, lty="dotted", col="grey88")
abline(h=lfay$majorTicks, lty="dashed", col="grey82")


# example showing volcano plot features
set.seed(123);
n <- 1000;
vdf <- data.frame(lfc=rnorm(n) * 2)
vdf$`-log10 (padj)` <- abs(vdf$lfc) * abs(rnorm(n))
plotSmoothScatter(vdf, xaxt="n", yaxt="n", xlab="Fold change",
   main="Volcano plot\ndisplayBase=2")
logFoldAxis(1)
pvalueAxis(2)


plotSmoothScatter(vdf, xaxt="n", yaxt="n", xlab="Fold change",
   main="Volcano plot\ndisplayBase=10")
logFoldAxis(1, displayBase=10)
pvalueAxis(2)
```
