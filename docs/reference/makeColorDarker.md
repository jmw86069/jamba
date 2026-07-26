# make R colors darker (or lighter)

Makes R colors darker or lighter based upon darkFactor

## Usage

``` r
makeColorDarker(
  hexColor,
  darkFactor = 2,
  sFactor = 1,
  fixAlpha = NULL,
  verbose = FALSE,
  keepNA = FALSE,
  useMethod = 1,
  ...
)
```

## Arguments

- hexColor:

  `character` vector of colors to adjust

- darkFactor:

  `numeric` value to adjust darkness, values above 1 make the color
  darker, values below 1 (or below 0) make the color brighter.

- sFactor:

  `numeric` value to adjust saturation, values above 1 become more
  saturated.

- fixAlpha:

  `numeric`, default NULL, to assign a fixed alpha transparency value,
  where 0 is transparent and 1 is opaque.

- verbose:

  `logical` indicating whether to print verbose output.

- keepNA:

  `logical`, default FALSE, whether to keep NA values as NA values in
  the output, otherwise NA values are considered grey input.

- useMethod:

  `integer` with two alternate methods, `1` is default.

- ...:

  Additional arguments are ignored.

## Value

`character` vector of R colors.

## Details

This function was originally intended to create border colors, or to
create slightly darker colors used for labels. It is also useful for for
making colors lighter, in adjusting color saturation up or down, or
applying alpha transparency during the same step.

Note when colors are brightened beyond value=1, the saturation is
gradually reduced in order to produce a visibly lighter color. The
saturation minimu is set to 0.2, to maintain at least some amount of
color.

## See also

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
[`rainbow2()`](https://jmw86069.github.io/jamba/reference/rainbow2.md),
[`rgb2col()`](https://jmw86069.github.io/jamba/reference/rgb2col.md),
[`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md),
[`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md),
[`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md),
[`unalpha()`](https://jmw86069.github.io/jamba/reference/unalpha.md),
[`warpRamp()`](https://jmw86069.github.io/jamba/reference/warpRamp.md)

## Examples

``` r
colorV <- c("red","orange","purple","blue");
colorVdark2 <- makeColorDarker(colorV, darkFactor=2);
colorVlite2 <- makeColorDarker(colorV, darkFactor=-2);
showColors(cexCellnote=0.7,
   list(
   `darkFactor=2`=colorVdark2,
   `original colors`=colorV,
   `darkFactor=-2`=colorVlite2
   ));


# these adjustments work really well inside a network diagram
# when coloring nodes, and providing an outline of comparable
# color.
plot(x=c(1,2,1,2), y=c(1,2,2,1), pch=21,
   xaxt="n", yaxt="n", xlab="", ylab="",
   xlim=c(0.5,2.5), ylim=c(0.5,2.5),
   bg=colorV, col=colorVdark2, cex=4, lwd=2);
graphics::points(x=c(1,2,1,2), y=c(1,2,2,1), pch=20, cex=4,
   col=colorVlite2);

# Making a color lighter can make it easier to add labels
# The setTextContrastColor() function also helps.
graphics::text(x=c(1,2,1,2), y=c(1,2,2,1), 1:4,
   col=setTextContrastColor(colorVlite2));

```
