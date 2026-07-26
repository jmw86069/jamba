# Simple rainbow palette replacement

Simple rainbow palette replacement using variable saturation and
vibrance

## Usage

``` r
rainbow2(n, s = c(0.9, 0.7, 0.88, 0.55), v = c(0.92, 1, 0.85, 0.94), ...)
```

## Arguments

- n:

  `integer` number of colors requested

- s, v:

  `numeric` vector of values to recycle as saturation and vibrance,
  respectively. The purpose is to improve visual distinction between
  adjacent and nearby colors in the color wheel.

- ...:

  additional arguments are passed to
  [`grDevices::rainbow()`](https://rdrr.io/r/grDevices/palettes.html):

  - `start`,`end` to control the starting and ending hue `[0,1]`,

  - `alpha` for alpha opacity, default NULL adds no alpha,

  - `rev` to reverse the color order.

## Value

`character` vector of R colors.

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
[`rgb2col()`](https://jmw86069.github.io/jamba/reference/rgb2col.md),
[`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md),
[`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md),
[`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md),
[`unalpha()`](https://jmw86069.github.io/jamba/reference/unalpha.md),
[`warpRamp()`](https://jmw86069.github.io/jamba/reference/warpRamp.md)

## Examples

``` r
showColors(list(
   `rainbow(24)`=grDevices::rainbow(24),
   `rainbow2(24)`=rainbow2(24),
   `rainbow2(24, rev=TRUE)`=rainbow2(24, rev=TRUE),
   `rainbow2(24, start=0.5, end=0.499)`=rainbow2(24,
      start=0.5, end=0.5-1e-5),
   `rainbow2(24, rev=TRUE,\nstart=0.5, end=0.499)`=rainbow2(24,
      rev=TRUE, start=0.5, end=0.5-1e-5)))

```
