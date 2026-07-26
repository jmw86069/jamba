# Determine square root axis tick mark positions

Determine square root axis tick mark positions, including positive and
negative range values.

## Usage

``` r
sqrtAxis(
  side = 1,
  x = NULL,
  pretty.n = 10,
  u5.bias = 1,
  big.mark = ",",
  plot = TRUE,
  las = 2,
  cex.axis = 0.6,
  ...
)
```

## Arguments

- side:

  `integer` value indicating the axis position, as used by
  [`graphics::axis()`](https://rdrr.io/r/graphics/axis.html), 1=bottom,
  2=left, 3=top, 4=right. Note that when `x` is supplied, the numeric
  range is defined using values in `x` and not the axis side.

- x:

  optional `numeric` vector representing the numeric range to be
  labeled. When supplied, the numeric range of `x` is used and not the
  axis side.

- pretty.n:

  `numeric` value indicating the number of desired tick marks, passed to
  [`pretty()`](https://rdrr.io/r/base/pretty.html).

- u5.bias:

  `numeric` value passed to
  [`pretty()`](https://rdrr.io/r/base/pretty.html) to influence the
  frequency of intermediate tick marks.

- big.mark:

  `character` value passed to
  [`format()`](https://rdrr.io/r/base/format.html) which helps visually
  distinguish numbers larger than 1000.

- plot:

  `logical` indicating whether to plot the axis tick marks and labels.

- las, cex.axis:

  `numeric` values passed to
  [`graphics::axis()`](https://rdrr.io/r/graphics/axis.html) when
  drawing the axis. The custom default `las=2` plots labels rotated
  perpendicular to the axis.

- ...:

  additional parameters are passed to
  [`pretty()`](https://rdrr.io/r/base/pretty.html), and to
  [`graphics::axis()`](https://rdrr.io/r/graphics/axis.html) when
  `plot=TRUE`.

## Value

invisible `numeric` vector with axis positions, named by normal space
numeric labels. The primary use is to add numeric axis tick marks and
labels.

## Details

This function calculates positions for tick marks for data that has been
transformed with [`sqrt()`](https://rdrr.io/r/base/MathFun.html),
specifically a directional transformation like `sqrt(abs(x)) * sign(x)`.

If `x` is supplied, it is used to define the numeric range, otherwise
the observed range is taken based upon `side`. If neither `x` nor `side`
is supplied, or if the numeric range is empty or zero width, it returns
'NULL'.

The main goal of this function is to provide reasonably placed tick
marks using integer values.

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
[`usrBox()`](https://jmw86069.github.io/jamba/reference/usrBox.md)

## Examples

``` r
plot(-3:3*10, -3:3*10, xaxt="n")
x <- sqrtAxis(1)
abline(v=x, col="grey", lty="dotted")
abline(h=pretty(par("usr")[3:4]), col="grey", lty="dotted")


# slightly different label placement with u5.bias=0
plot(-3:3*10, -3:3*10, xaxt="n")
x <- sqrtAxis(1, u5.bias=0)
abline(v=x, col="grey", lty="dotted")
abline(h=pretty(par("usr")[3:4]), col="grey", lty="dotted")

```
