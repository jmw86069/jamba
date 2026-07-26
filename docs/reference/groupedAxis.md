# Draw grouped axis labels

Draw grouped axis labels given a character vector.

## Usage

``` r
groupedAxis(
  side = 1,
  x,
  group_style = c("partial_grouped", "grouped", "centered"),
  las = 2,
  returnFractions = TRUE,
  nudge = 0.2,
  do_abline = FALSE,
  abline_lty = "solid",
  abline_col = "grey40",
  do_plot = TRUE,
  ...
)
```

## Arguments

- side:

  `integer` indicating the axis side, passed to
  [`graphics::axis()`](https://rdrr.io/r/graphics/axis.html). 1=bottom,
  2=left, 3=top, 4=right.

- x:

  `character` vector of axis labels

- group_style:

  `character` string indicating the style of label:

  - `"partial_grouped"` - uses square bracket to bound 2+ repeated
    entries, and single line tick mark for non-repeated entries.

  - `"grouped"` - uses square bracket to bound each set of repeated
    entries including non-repeated entries.

  - `"centered"` - only labels the center of each group of repeated
    entries with no bracket bounding the entries.

- las:

  `integer` indicating whether labels should be perpendicular, see
  `graphics::par("las")`.

- returnFractions:

  `logical` passed to
  [`breaksByVector()`](https://jmw86069.github.io/jamba/reference/breaksByVector.md)
  to calculate label positions. Set `returnFractions=FALSE` and all
  labels will only appear at integer locations on the axis.

- nudge:

  `numeric` adjustment for labels away from the plot border.

- do_abline:

  `logical` indicating whether to draw
  [`graphics::abline()`](https://rdrr.io/r/graphics/abline.html) lines
  inside the plot to indicate the exact breakpoints between each group
  of labels.

- abline_lty:

  line type compatible with `graphics::par("lty")`, used when
  `do_abline=TRUE`.

- abline_col:

  `character` color used when `do_abline=TRUE`.

- do_plot:

  `logical` whether to plot the resulting axis, as an option to suppress
  the output and do something else with the `data.frame` of coordinates
  returned by this function.

- ...:

  additional arguments are passed to
  [`breaksByVector()`](https://jmw86069.github.io/jamba/reference/breaksByVector.md),
  and/or to [`graphics::axis()`](https://rdrr.io/r/graphics/axis.html).

## Value

`data.frame` invisibly, which contains the relevant axis coordinates,
labels, and whether the coordinate should appear with a tick mark.

## Details

This function extends
[`breaksByVector()`](https://jmw86069.github.io/jamba/reference/breaksByVector.md)
specifically for axis labels. It is intended where character labels are
spaced at integer steps, and some labels are expected to be repeated.

## See also

Other jam plot functions:
[`adjustAxisLabelMargins()`](https://jmw86069.github.io/jamba/reference/adjustAxisLabelMargins.md),
[`coordPresets()`](https://jmw86069.github.io/jamba/reference/coordPresets.md),
[`decideMfrow()`](https://jmw86069.github.io/jamba/reference/decideMfrow.md),
[`drawLabels()`](https://jmw86069.github.io/jamba/reference/drawLabels.md),
[`getPlotAspect()`](https://jmw86069.github.io/jamba/reference/getPlotAspect.md),
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
withr::with_par(list("mar"=c(4,4,6,6)), {
b <- rep(LETTERS[1:5], c(2,3,5,4,3));
b2 <- c(b[1:2], makeNames(b[3:5]), b[6:16]);
nullPlot(doBoxes=FALSE,
   doUsrBox=TRUE,
   xlim=c(0,18),
   ylim=c(0,18));

groupedAxis(1, b);
groupedAxis(2, b, group_style="grouped");
groupedAxis(2, b, group_style="centered");
groupedAxis(3, b2, do_abline=TRUE);
groupedAxis(4, b2, group_style="grouped");
graphics::mtext(side=1, "group_style='partial_grouped'", line=2, las=0);
graphics::mtext(side=2, "group_style='grouped'", line=2, las=0);
graphics::mtext(side=3, "group_style='partial_grouped'", line=2, las=0);
graphics::mtext(side=4, "group_style='grouped'", line=2, las=0);
})

```
