# Draw text with shadow border

Draw text with shadow border

## Usage

``` r
shadowText(
  x,
  y = NULL,
  labels = NULL,
  col = "white",
  bg = setTextContrastColor(col),
  r = getOption("jam.shadow.r", 0.15),
  offset = c(0.15, -0.15),
  n = getOption("jam.shadow.n", 8),
  outline = getOption("jam.outline", TRUE),
  alphaOutline = getOption("jam.alphaOutline", 0.4),
  shadow = getOption("jam.shadow", FALSE),
  shadowColor = getOption("jam.shadowColor", "black"),
  alphaShadow = getOption("jam.alphaShadow", 0.2),
  shadowOrder = c("each", "all"),
  cex = graphics::par("cex"),
  font = graphics::par("font"),
  doTest = FALSE,
  ...
)
```

## Arguments

- x, y:

  numeric coordinates, either as vectors x and y, or x as a two-color
  matrix recognized by
  [`grDevices::xy.coords()`](https://rdrr.io/r/grDevices/xy.coords.html).

- labels:

  vector of labels to display at the corresponding xy coordinates.

- col, bg, shadowColor:

  the label color, and background (outline) color, and shadow color (if
  `shadow=TRUE`), for each element in `labels`. Colors are applied in
  order, and recycled to `length(labels)` as needed. By default `bg`
  will choose a contrasting color, based upon
  [`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md).
  Also by default, the shadow is "black" true to its name, since it is
  expected to darken the area around it.

- r:

  the outline radius, expressed as a fraction of the width of the
  character "A" as returned by
  [`graphics::strwidth()`](https://rdrr.io/r/graphics/strwidth.html).

- offset:

  the outline offset position in xy coordinates, expressed as a fraction
  of the width of the character "A" as returned by
  [`graphics::strwidth()`](https://rdrr.io/r/graphics/strwidth.html),
  and
  [`graphics::strheight()`](https://rdrr.io/r/graphics/strwidth.html),
  respectively. The offset is only applied when `shadow=TRUE` to enable
  the shadow effect.

- n:

  `numeric` steps around the label used to create the outline. A higher
  number may be useful for very large font sizes, otherwise 8 is a
  reasonably good balance between detail and the number of labels added.

- outline:

  `logical` whether to enable outline drawing.

- alphaOutline, alphaShadow:

  `numeric` alpha transparency to use for the outline and shadow colors,
  respectively.

- shadow:

  `logical` whether to enable shadow drawing.

- shadowOrder:

  `character` value indicating when shadows are drawn relative to
  drawing labels: `"each"` draws each shadow with each label, so that
  shadows will overlap previous labels; `"all"` draws all shadows first
  then all labels, so labels will always appear above all shadows. See
  examples.

- cex:

  `numeric` scalar applied to font size, default `graphics::par("cex")`.

- font:

  `character` applied to font family, default `graphics::par("font")`.

- doTest:

  `logical` whether to create a visual example of output. Note that it
  calls
  [`usrBox()`](https://jmw86069.github.io/jamba/reference/usrBox.md) to
  color the plot area, and the background can be overridden with
  something like 'fill="navy"'.

- ...:

  other parameters are passed to
  [`text`](https://rdrr.io/r/graphics/text.html). Note that certain
  parameters are not vectorized in that function, such as 'srt' which
  requires only a fixed value. To rotate each label independently,
  multiple calls to [`text`](https://rdrr.io/r/graphics/text.html) or
  `shadowText` must be made. Other parameters like `adj` only accept up
  to two values, and those two values affect all label positioning.

## Value

invisible `list` of components used to call
[`graphics::text()`](https://rdrr.io/r/graphics/text.html), including:
x, y, allColors, allLabels, cex, font.

## Details

Draws text with the same syntax as
[`graphics::text()`](https://rdrr.io/r/graphics/text.html) except that
this function adds a contrasting color border around the text, which
helps visibility when the background color is either not known, or is
not expected to be a fixed contrasting color.

The function draws the label n times with the chosed background color,
then the label itself atop the background text. It does not typically
have a noticeable effect on rendering time, but it may impact downstream
uses in vector file formats like 'SVG' and 'PDF', where text is stored
as proper text and font objects. Take care when editing text that the
underlying shadow text is also edited in sync.

The parameter `doTest=TRUE` will display a visual example. The
background color can be modified with `fill="navy"` for example.

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
[`shadowText_options()`](https://jmw86069.github.io/jamba/reference/shadowText_options.md),
[`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md),
[`sqrtAxis()`](https://jmw86069.github.io/jamba/reference/sqrtAxis.md),
[`usrBox()`](https://jmw86069.github.io/jamba/reference/usrBox.md)

## Examples

``` r
shadowText(doTest=TRUE);

shadowText(doTest=TRUE, fill="navy");

shadowText(doTest=TRUE, fill="red4");


# example showing labels with overlapping shadows
withr::with_par(list("mfrow"=c(1, 2)), {
nullPlot(doBoxes=FALSE);
graphics::title(main="shadowOrder='each'");
shadowText(x=c(1.5, 1.65), y=c(1.5, 1.55),
   labels=c("one", "two"), cex=c(2, 4), shadowOrder="each")
nullPlot(doBoxes=FALSE);
graphics::title(main="shadowOrder='all'");
shadowText(x=c(1.5, 1.65), y=c(1.5, 1.55),
   labels=c("one", "two"), cex=c(2, 4), shadowOrder="all")
})

```
