# Adjust axis label margins

Adjust axis label margins to accommodate axis labels

## Usage

``` r
adjustAxisLabelMargins(
  x,
  margin = 1,
  maxFig = 1/2,
  cex = graphics::par("cex"),
  cex.axis = graphics::par("cex.axis"),
  prefix = "-- -- ",
  ...
)
```

## Arguments

- x:

  `character` vector of axis labels

- margin:

  `integer` value indicating which margin to adjust, using the order by
  `graphics::par("mar")`, 1=bottom, 2=left, 3=top, 4=right.

- maxFig:

  `numeric` fraction less than 1, indicating the maximum size of margin
  relative to the figure size. Setting margins too large results in an
  error otherwise.

- cex:

  `numeric` or NULL, default `graphics::par("cex")`, used as a
  convenience with `cex * cex.axis` passed to
  [`graphics::strwidth()`](https://rdrr.io/r/graphics/strwidth.html).
  However, [`graphics::axis()`](https://rdrr.io/r/graphics/axis.html)
  itself should use `cex.axis` when adjusting axis label font size.

- cex.axis:

  `numeric`, default `graphics::par("cex.axis")` to define the axis
  label font size.

- prefix:

  `character` string to add whitespace around the axis label in order to
  add a "buffer" of whitespace.

- ...:

  additional parameters are ignored.

## Value

`list` named "mai" suitable for use in
[`graphics::par()`](https://rdrr.io/r/graphics/par.html) to adjust
margin size using in inches.

## Details

This function takes a vector of axis labels, and the margin where they
will be used, and adjusts the relevant axis margin to accomodate the
label size, up to a maximum fraction of the figure size as defined by
`maxFig`.

Labels are assumed to be perpendicular to the axis, for example argument
`las=2` when using
[`graphics::text()`](https://rdrr.io/r/graphics/text.html).

Note this function does not render labels in the figure, and therefore
does not revert axis margins to their original size. That process should
be performed separately.

## See also

Other jam plot functions:
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
[`sqrtAxis()`](https://jmw86069.github.io/jamba/reference/sqrtAxis.md),
[`usrBox()`](https://jmw86069.github.io/jamba/reference/usrBox.md)

## Examples

``` r
xlabs <- paste0("item_", (1:20));
ylabs <- paste0("rownum_", (1:20));

# proper adjustment should be done using withr, for example
x_cex <- 0.8;
y_cex <- 1.2;
withr::with_par(adjustAxisLabelMargins(xlabs, 1, cex.axis=x_cex), {
   withr::local_par(adjustAxisLabelMargins(ylabs, 2, cex.axis=y_cex))
   nullPlot(xlim=c(1,20), ylim=c(1,20), doMargins=FALSE);
   graphics::axis(1, at=1:20, labels=xlabs, las=2, cex.axis=x_cex);
   graphics::axis(2, at=1:20, labels=ylabs, las=2, cex.axis=y_cex);
})


withr::with_par(adjustAxisLabelMargins(xlabs, 3, cex.axis=x_cex), {
   withr::local_par(adjustAxisLabelMargins(ylabs, 4, cex.axis=y_cex))
   nullPlot(xlim=c(1,20), ylim=c(1,20), doMargins=FALSE);
   graphics::axis(3, at=1:20, labels=xlabs, las=2);
   graphics::axis(4, at=1:20, labels=ylabs, las=2);
})


par("mar")
#> [1] 4.809679 4.100000 4.809679 2.100000
```
