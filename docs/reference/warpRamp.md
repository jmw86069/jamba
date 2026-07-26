# Warp colors in a color ramp

Warp colors in a color ramp

## Usage

``` r
warpRamp(
  ramp,
  lens = 5,
  divergent = TRUE,
  expandFactor = 10,
  plot = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- ramp:

  character vector of R colors

- lens:

  numeric lens factor, centered at zero, where positive values cause
  colors to change more rapidly near zero, and negative values cause
  colors to change less rapidly near zero and more rapidly near the
  extreme.

- divergent:

  logical indicating whether the `ramp` represents divergent colors,
  which are assumed to be symmetric above and below zero. Otherwise,
  colors are assumed to begin at zero.

- expandFactor:

  numeric factor used to expand the color ramp prior to selecting the
  nearest warped numeric value as the result of
  [`warpAroundZero()`](https://jmw86069.github.io/jamba/reference/warpAroundZero.md).
  This value should not need to be changed unless the lens is extremely
  high (\>100).

- plot:

  logical indicating whether to plot the input and output color ramps
  using
  [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md).

- verbose:

  logical indicating whether to print verbose output.

- ...:

  additional parameters are passed to
  [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md).

## Value

`character` vector of R colors, with the same length as the input vector
`ramp`.

## Details

This function takes a vector of colors in a color ramp (color gradient)
and warps the gradient using a lens factor. The effect causes the color
gradient to change faster or slower, dependent upon the lens factor.

The main intent is for heatmap color ramps, where the color gradient
changes are not consistent with meaningful numeric differences being
shown in the heatmap. In short, this function enhances colors.

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
[`makeColorDarker()`](https://jmw86069.github.io/jamba/reference/makeColorDarker.md),
[`rainbow2()`](https://jmw86069.github.io/jamba/reference/rainbow2.md),
[`rgb2col()`](https://jmw86069.github.io/jamba/reference/rgb2col.md),
[`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md),
[`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md),
[`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md),
[`unalpha()`](https://jmw86069.github.io/jamba/reference/unalpha.md)

## Examples

``` r
BuRd <- rev(RColorBrewer::brewer.pal(11, "RdBu"));
BuRdPlus5 <- warpRamp(BuRd, lens=2, plot=TRUE);

BuRdMinus5 <- warpRamp(BuRd, lens=-2, plot=TRUE);


Reds <- RColorBrewer::brewer.pal(9, "Reds");
RedsL <- lapply(nameVector(c(-10,-5,-2,0,2,5,10)), function(lens){
   warpRamp(Reds, lens=lens, divergent=FALSE)
});
showColors(RedsL);

```
