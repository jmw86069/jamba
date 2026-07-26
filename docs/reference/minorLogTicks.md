# Calculate major and minor tick marks for log-scale axis

Calculate major and minor tick marks for log-scale axis

## Usage

``` r
minorLogTicks(
  side = NULL,
  lims = NULL,
  logBase = 2,
  displayBase = 10,
  logStep = 1,
  minorWhich = c(2, 5),
  asValues = TRUE,
  offset = 0,
  symmetricZero = (offset > 0),
  col = "black",
  col.ticks = col,
  combine = FALSE,
  logAxisType = c("normal", "flip", "pvalue"),
  verbose = FALSE,
  ...
)
```

## Arguments

- side:

  `integer` value indicating which axis to produce tick marks, 1=bottom,
  2=left, 3=top, 4=right.

- lims:

  `numeric` vector length=2, indicating specific numeric range to use
  for tick marks.

- logBase:

  `numeric` value indicating the logarithmic base, assumed to be applied
  to the numeric `lims` limits, or the axis range, previously.

- displayBase:

  `numeric` value indicating the base used to position axis labels,
  typically `displayBase=10` is used to draw labels at typical
  positions.

- logStep:

  `integer` value indicating the number of log steps between major axis
  label positions. Typically `logStep=1` will draw a label every log
  position based upon `displayBase`, for example `displayBase=10` and
  `logStep=1` will use `c(1,10,100,1000)`; and `displayBase=10` and
  `logStep=2` would use `c(1,100,10000)`.

- minorWhich:

  `integer` vector of values to label, where those integer values are
  between 1 and `displayBase`, for example `displayBase=10` may label
  only `c(2,5)`, which implies minor tick labels at
  `c(2, 5, 20, 50, 200, 500)`. Any minor labels which would otherwise
  equal a major tick position are removed. By default, when
  `displayBase=2`, `minorWhich=c(1.5)` which has the effect of drawing
  one minor label between each two-fold major tick label.

- asValues:

  `logical` indicating whether to create exponentiated numeric labels.
  When `asValues=FALSE`, it creates `expression` objects which include
  the exponential value. Use `asValues=FALSE` and `logAxisType="pvalue"`
  to draw P-value labels.

- offset:

  `numeric` value added during log transformation, typically of the form
  `log(1 + x)` where `offset=1`. The offset is used to determine the
  accurate numeric label such that values of `0` are properly labeled by
  the original numeric value.

- symmetricZero:

  `logical` indicating whether numeric values are symmetric around zero.
  For example, log fold changes should use `symmetricZero=TRUE` which
  ensures a log2 value of `-2` is labeled `-4` to indicate a negative
  four fold change. If `symmetricZero=FALSE` a log2 value of `-2` would
  be labeled `0.0625`.

- col, col.ticks:

  `character` color used for the axis label, and axis tick marks,
  respectively, default "black".

- combine:

  `logical`, default FALSE, whether to combine major and minor ticks
  into one continuous set of major tick marks.

- logAxisType:

  `character` string indicating the type of log axis:

  - normal: typical axis style and orientation

  - flipped: used for reverse orientation

  - pvalue: used for `-log10(pvalue)` orientation.

- verbose:

  `logical` whether to print verbose output.

- ...:

  additional parameters are ignored.

## Value

`list` of axis tick positions, and corresponding labels, for major and
minor ticks. Note that labels may be `numeric`, `character`, or
`expression`. Specifically when `expression` the
[`graphics::axis()`](https://rdrr.io/r/graphics/axis.html) must be
called once per label. Ticks and labels are restricted to the 'lims'
range rounded up to the nearest integer.

- majorTicks: `numeric` position of each major tick mark

- minorTicks: `numeric` position of each minor tick mark

- allTicks: `numeric` position of each major tick mark

- majorLabels: label to show for each tick mark

- minorLabels: label to show for each tick mark

- minorSet: the `numeric` steps requested for minor ticks

- minorWhich: the `numeric` steps requested for minor labels

- allLabelsDF: `data.frame` with all tick marks and labels, with colname
  `"use"` indicating whether the label is displayed beside each tick
  mark.

## Details

This function is called by
[`minorLogTicksAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md),
and it may be better to use that function, or
[`logFoldAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md)
or
[`pvalueAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md)
which has better preset options.

This function calculates log units for the axis of an existing base R
plot. It calculates appropriate tick and label positions for:

- major steps, which are typically in log steps; and

- minor steps, which are typically a subset of steps at one lower log
  order.

For example, log 10 steps would be: `c(1, 10, 100, 1000)`, and minor
steps would be `c(2, 5, 20, 50, 200, 500, 2000, 5000)`.

### Motivation

This function is motivated to fill a few difficult cases:

1.  Label axis ticks properly when used together with `offset`. For
    example `log2(1 + x)` uses `offset=1`. Other offsets can be used as
    relevant.

2.  Create axis labels which indicate negative fold change values, for
    example `-2` in log2 fold change units would be labeled with fold
    change `-4`, and not `0.0625`.

3.  Use symmetric tick marks around x=0 when applied to log fold
    changes.

4.  Display actual P-values when plotting `log10(Pvalue)`, which is
    common for volcano plots.

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
[`log2signed()`](https://jmw86069.github.io/jamba/reference/log2signed.md),
[`middle()`](https://jmw86069.github.io/jamba/reference/middle.md),
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
## This example shows how to draw axis labels manually,
## but the function minorLogTicksAxis() is easier to use.
xlim <- c(0,4);
nullPlot(xlim=xlim, doMargins=FALSE);
mlt <- minorLogTicks(1,
   logBase=10,
   offset=1,
   minTick=0);
maj <- subset(mlt$allLabelsDF, type %in% "major");
graphics::axis(1, las=2,
   at=maj$tick, label=maj$text);
min <- subset(mlt$allLabelsDF, type %in% "minor");
graphics::axis(1, las=2, cex.axis=0.7,
   at=min$tick, label=min$text,
   col="blue");
graphics::text(x=log10(1+c(0,5,50,1000)), y=rep(1.7, 4),
   label=c(0,5,50,1000), srt=90);


nullPlot(xlim=c(-4,10), doMargins=FALSE);
abline(v=0, lty=2)
graphics::axis(3, las=2);
minorLogTicksAxis(1, logBase=2, displayBase=10, symmetricZero=TRUE);


nullPlot(xlim=c(-4,10), doMargins=FALSE);
graphics::axis(3, las=2);
minorLogTicksAxis(1, logBase=2, displayBase=10, offset=1);
x2 <- stats::rnorm(1000) * 40;
d2 <- stats::density(log2(1+abs(x2)) * ifelse(x2<0, -1, 1));
lines(x=d2$x, y=normScale(d2$y)+1, col="green4");


nullPlot(xlim=c(0,10), doMargins=FALSE);
graphics::axis(3, las=2);
minorLogTicksAxis(1, logBase=2, displayBase=10, offset=1);
x1 <- c(0, 5, 15, 200);
graphics::text(y=rep(1.0, 4), x=log2(1+x1), label=x1, srt=90, adj=c(0,0.5));
graphics::points(y=rep(0.95, 4), x=log2(1+x1), pch=20, cex=2, col="blue");

```
