# Show colors from a vector or list

Show colors from a vector or list

## Usage

``` r
showColors(
  x,
  labelCells = NULL,
  transpose = FALSE,
  srtCellnote = NULL,
  adjustMargins = TRUE,
  makeUnique = FALSE,
  doPlot = TRUE,
  ...
)
```

## Arguments

- x:

  one of these input types:

  - `character` vector of colors

  - `list` with any combination of `character` or `function`. Each
    element of the `list` is displayed on its own row. List names are
    shown on the y-axis.

  - `function`: color function in one of two formats:

    1.  [`circlize::colorRamp2()`](https://rdrr.io/pkg/circlize/man/colorRamp2.html)
        which defines numeric breaks, and one color *at* each break.
        This function is used by ComplexHeatmap and is unique and useful
        in defining the color at each break and not in between each
        break. The benefit is that a specific color is known to mean
        exactly the numeric value, instead of assigning the color to
        some intermediate mean of adjacent breaks, then interpolating
        the color between them.

    2.  `function` as defined in color packages such as
        `viridis::viridis(10)` where the number `10` defines the number
        of colors to produce. For these functions, colors are displayed
        ranging from 0 to 1, intending to mean lowest (0) to highest (1)
        color, with n steps.

  - 'ggplot' or 'ggproto' object, see Details.

- labelCells:

  `logical` whether to label colors atop the color itself. If NULL
  (default) it will only display labels with 40 or fewer items on either
  axis.

- transpose:

  `logical` whether to transpose the colors to display top-to-bottom,
  instead of left-to-right.

- srtCellnote:

  `numeric` angle to rotate text when `labelCells=TRUE`. When set to
  NULL, labels are vertical srtCellnote=90 when `transpose=FALSE` and
  horizontal srtCellnote=0 when `transpose=TRUE`.

- adjustMargins:

  `logical` indicating whether to call
  [`adjustAxisLabelMargins()`](https://jmw86069.github.io/jamba/reference/adjustAxisLabelMargins.md)
  to adjust the x- and y-axis label margins to accomodate the label
  size.

  - Note when an axis is hidden by using `xaxt="n"` or `xaxt="n"`, the
    respective margin will not be adjusted.

  - The arguments in `...` take precedence over
    [`graphics::par()`](https://rdrr.io/r/graphics/par.html), when
    deciding whether to adjust margins. However if `xaxt="s"` and
    `graphics::par("xaxt"="n")` the margin will be adjusted but not
    displayed. In this way the axes can be adjusted without displaying
    the labels, so the labels can be rendered later if needed.

- makeUnique:

  `logical` indicating whether to display only the first unique color.
  When `x` is supplied as a `list` this operation will display the first
  unique color for each `list` element. Also, when `x` is a `list`, just
  to be fancy, `makeUnique` is recycled to `length(x)` so certain list
  elements can display unique values, while others display all values.

- doPlot:

  `logical` indicating whether to produce a visual plot. Note this
  function returns the color matrix invisibly.

- ...:

  additional parameters are passed to
  [`imageByColors()`](https://jmw86069.github.io/jamba/reference/imageByColors.md).

## Value

invisible color `matrix` used by
[`imageByColors()`](https://jmw86069.github.io/jamba/reference/imageByColors.md).
When the input `x` is empty, or cannot be converted to colors when `x`
contains a `function`, the output returns 'NULL'.

## Details

This function simply displays colors for review, using
[`imageByColors()`](https://jmw86069.github.io/jamba/reference/imageByColors.md)
to display colors and labels across the plot space.

When supplied a `list`, each row in
[`imageByColors()`](https://jmw86069.github.io/jamba/reference/imageByColors.md)
represents an entry in the `list`. Nothing fancy.

When input is `function`, it is assumed to be one of these formats:

1.  `viridis::viridis(n)`

    - Sequential colors with `n` steps representing colors from lowest
      to highest value.

    - This format does not apply any numeric range, no numeric threshold
      is implied in the function at all. It simply takes its internal
      range of colors and produces `n` output colors, usually using
      [`colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html)
      to interpolate intermediate colors when needed.

    - Common examples are R packages such as 'viridis' which provide
      several different color gradients.

2.  [`circlize::colorRamp2()`](https://rdrr.io/pkg/circlize/man/colorRamp2.html)

    - Colors associated with specific `numeric` values.

    - A distinctive feature of `colorRamp2()` color `function` is that
      it assigns a vector of 'colors' to a vector of `numeric` values.
      This mechanism is important when it is useful to know exactly what
      `numeric` value is represented by a color.

    - A common alternative is to assign colors *between* `numeric`
      breaks, in which case the defined color is associated with an
      intermediate value betweenbreaks.

3.  `ggplot` or `ggproto` scales object produced by 'ggplot2'.

    - Given a
      [`ggplot2::ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
      object, it will use 'colour' or 'fill' aesthetics present in the
      'mapping', only when there is an explicit color assignment. It
      does not currently determine the default color aesthetic function
      to use.

    - A `ggproto` object produced by a color scales function, for
      example
      [`ggplot2::scale_color_discrete()`](https://ggplot2.tidyverse.org/reference/scale_colour_discrete.html),
      for aesthetics 'color', 'colour', or 'fill'.

    - It makes reasonable attempt to recognize custom limits, for
      example '\_gradientn()' functions which may have specific 'values'
      (scaled from 0 to 1) buthent applied to specific 'limits' (scaled
      per user coordinates).

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
[`sqrtAxis()`](https://jmw86069.github.io/jamba/reference/sqrtAxis.md),
[`usrBox()`](https://jmw86069.github.io/jamba/reference/usrBox.md)

Other jam color functions:
[`alpha2col()`](https://jmw86069.github.io/jamba/reference/alpha2col.md),
[`applyCLrange()`](https://jmw86069.github.io/jamba/reference/applyCLrange.md),
[`col2alpha()`](https://jmw86069.github.io/jamba/reference/col2alpha.md),
[`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md),
[`col2hsl()`](https://jmw86069.github.io/jamba/reference/col2hsl.md),
[`col2hsv()`](https://jmw86069.github.io/jamba/reference/col2hsv.md),
[`color2gradient()`](https://jmw86069.github.io/jamba/reference/color2gradient.md),
[`fixYellow()`](https://jmw86069.github.io/jamba/reference/fixYellow.md),
[`fixYellowHue()`](https://jmw86069.github.io/jamba/reference/fixYellowHue.md),
[`getColorRamp()`](https://jmw86069.github.io/jamba/reference/getColorRamp.md),
[`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md),
[`hsl2col()`](https://jmw86069.github.io/jamba/reference/hsl2col.md),
[`hsv2col()`](https://jmw86069.github.io/jamba/reference/hsv2col.md),
[`isColor()`](https://jmw86069.github.io/jamba/reference/isColor.md),
[`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md),
[`makeColorDarker()`](https://jmw86069.github.io/jamba/reference/makeColorDarker.md),
[`rainbow2()`](https://jmw86069.github.io/jamba/reference/rainbow2.md),
[`rgb2col()`](https://jmw86069.github.io/jamba/reference/rgb2col.md),
[`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md),
[`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md),
[`unalpha()`](https://jmw86069.github.io/jamba/reference/unalpha.md),
[`warpRamp()`](https://jmw86069.github.io/jamba/reference/warpRamp.md)

## Examples

``` r
x <- color2gradient(list(Reds=c("red"), Blues=c("blue")), n=c(4,7));
showColors(x);


showColors(getColorRamp("firebrick3"))


if (requireNamespace("RColorBrewer", quietly=TRUE)) {
   RColorBrewer_namelist <- rownames(RColorBrewer::brewer.pal.info);
   y <- lapply(nameVector(RColorBrewer_namelist), function(i){
      n <- RColorBrewer::brewer.pal.info[i, "maxcolors"]
      j <- RColorBrewer::brewer.pal(n, i);
      nameVector(j, seq_along(j));
   });
   showColors(y, cexCellnote=0.6, cex.axis=0.7, main="Brewer Colors");
}

if (requireNamespace("viridisLite", quietly=TRUE)) {
   # given one function name it will display discrete colors
   showColors(viridisLite::viridis)
   # a list of functions will show each function output
   showColors(list(viridis=viridisLite::viridis,
      inferno=viridisLite::inferno))

   # grab the full viridis color map
   z <- rgb2col(viridisLite::viridis.map[,c("R","G","B")]);
   # split the colors into a list
   viridis_names <- c(A="magma",
      B="inferno",
      C="plasma",
      D="viridis",
      E="cividis",
      F="rocket",
      G="mako",
      H="turbo")
   y <- split(z,
      paste0(viridisLite::viridis.map$opt, ": ",
      viridis_names[viridisLite::viridis.map$opt]));
   showColors(y, labelCells=TRUE, xaxt="n", main="viridis.map colors");
}




# demonstrate makeUnique=TRUE
j1 <- getColorRamp("rainbow", n=7);
names(j1) <- seq_along(j1);
j2 <- rep(j1, each=3);
names(j2) <- makeNames(names(j2), suffix="_rep");
j2
#>      1_rep1      1_rep2      1_rep3      2_rep1      2_rep2      2_rep3 
#> "#FF0000FF" "#FF0000FF" "#FF0000FF" "#EEDD00FF" "#EEDD00FF" "#EEDD00FF" 
#>      3_rep1      3_rep2      3_rep3      4_rep1      4_rep2      4_rep3 
#> "#21FF00FF" "#21FF00FF" "#21FF00FF" "#00FFCCFF" "#00FFCCFF" "#00FFCCFF" 
#>      5_rep1      5_rep2      5_rep3      6_rep1      6_rep2      6_rep3 
#> "#0043FFFF" "#0043FFFF" "#0043FFFF" "#A900FFFF" "#A900FFFF" "#A900FFFF" 
#>      7_rep1      7_rep2      7_rep3 
#> "#FF0066FF" "#FF0066FF" "#FF0066FF" 
showColors(list(
   j1=j1,
   j2=j2,
   j3=j2),
   makeUnique=c(FALSE, FALSE, TRUE))

```
