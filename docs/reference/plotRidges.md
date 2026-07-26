# Plot ridges density plots for numeric matrix input

Plot ridges density plots for numeric matrix input

## Usage

``` r
plotRidges(
  x,
  xScale = c("none", "-log10", "log10"),
  xlab = NULL,
  ylab = NULL,
  title = ggplot2::waiver(),
  subtitle = ggplot2::waiver(),
  caption = ggplot2::waiver(),
  xlim = NULL,
  color_sub = NULL,
  rel_min_height = 0,
  bandwidth = NULL,
  adjust = 1,
  scale = 1,
  share_bandwidth = TRUE,
  ...
)
```

## Arguments

- x:

  `matrix` with numeric values, or a `list` of `numeric` vectors. In
  either case the data is converted to long-tall format before plotting.

- xScale:

  `character` string indicating whether to transform the x-axis values:

  - `"none"`: no transformation

  - `"-log10"`: values are transformed with `log10(x)` and x-axis labels
    are adjusted accordingly.

  - `"log10"`: values are transformed with `log10(1 + x)` except that
    negative values are transformed with `-log10(1 - x)`. The x-axis
    labels are plotted to account for the `log10(1 + x)` offset.

- xlab, ylab:

  `character` strings optionally used as x-axis and y-axis labels.

- title, subtitle, caption:

  `character` string values optionally passed to the relevant downstream
  `ggplot2` functions.

- xlim:

  passed to
  [`ggplot2::xlim()`](https://ggplot2.tidyverse.org/reference/lims.html)
  to define the x-axis range.

- color_sub:

  `character` vector named by `colnames(x)`, or when `x` is a `list`,
  `names(color_sub)` should contain `names(x)`, used to define specific
  colors for each ridge plot.

- rel_min_height:

  `numeric` values passed to
  [`ggridges::geom_density_ridges2()`](https://wilkelab.org/ggridges/reference/geom_density_ridges.html)

- bandwidth:

  `numeric` value used to define the bandwidth density when
  `share_bandwidth=TRUE` which is default. The bandwidth affects the
  level of detail presented in each ridgeline, and when shared across
  ridgelines `share_bandwidth=TRUE` then each ridgeline will use the
  same consistent level of detail. In this case, it is passed to
  [`ggridges::geom_density_ridges2()`](https://wilkelab.org/ggridges/reference/geom_density_ridges.html).
  Note when `bandwidth=NULL` a default value is derived from the range
  of data to be plotted.

- adjust:

  `numeric` used to adjust the default bandwidth only when
  `bandwidth=NULL`. It is intended as a convenient method to adjust the
  level of detail.

- scale:

  `numeric` passed directly to
  [`ggridges::geom_density_ridges2()`](https://wilkelab.org/ggridges/reference/geom_density_ridges.html).

- share_bandwidth:

  `logical` indicating whether to supply
  [`ggridges::geom_density_ridges2()`](https://wilkelab.org/ggridges/reference/geom_density_ridges.html)
  a specific `bandwidth` to use for all ridgelines. When
  `share_bandwidth=FALSE` then each ridgeline is presented using the
  default bandwidth in
  [`ggridges::geom_density_ridges2()`](https://wilkelab.org/ggridges/reference/geom_density_ridges.html).

- ...:

  additional arguments are ignored.

## Value

object with class `"gg", "ggplot"` with density plot in the form of
ridges.

## Details

This function is a convenient wrapper for
[`ggridges::geom_density_ridges2()`](https://wilkelab.org/ggridges/reference/geom_density_ridges.html),
intended to be analogous to
[`plotPolygonDensity()`](https://jmw86069.github.io/jamba/reference/plotPolygonDensity.md)
which differs by plotting each item in a separate plot panel using base
graphics. This function plots each item as a ridgeline plot in the same
plot window using
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

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
[`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md),
[`shadowText()`](https://jmw86069.github.io/jamba/reference/shadowText.md),
[`shadowText_options()`](https://jmw86069.github.io/jamba/reference/shadowText_options.md),
[`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md),
[`sqrtAxis()`](https://jmw86069.github.io/jamba/reference/sqrtAxis.md),
[`usrBox()`](https://jmw86069.github.io/jamba/reference/usrBox.md)

## Examples

``` r
# multiple columns
set.seed(123);
xm <- do.call(cbind, lapply(1:4, function(i){stats::rnorm(2000)}))
plotRidges(xm)


set.seed(123);
x <- stats::rnorm(2000)
plotRidges(x)
```
