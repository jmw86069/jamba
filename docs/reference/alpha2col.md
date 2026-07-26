# set R color alpha value

Define the alpha transparency per R color

## Usage

``` r
alpha2col(x, alpha = 1, maxValue = 1, ...)
```

## Arguments

- x:

  R compatible color, either a color name, or hex value, or a mixture of
  the two. Any value compatible with
  [`col2rgb`](https://rdrr.io/r/grDevices/col2rgb.html).

- alpha:

  numeric alpha transparency to use per x color. alpha is recycled to
  length(x) as needed.

- maxValue:

  numeric maximum value to return, useful when the downstream alpha
  range should be 255. By default maxValue=1 is returned.

- ...:

  Additional arguments are ignored.

## Value

`character` vector of R colors, with alpha values.

## See also

Other jam color functions:
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
[`unalpha()`](https://jmw86069.github.io/jamba/reference/unalpha.md),
[`warpRamp()`](https://jmw86069.github.io/jamba/reference/warpRamp.md)

## Examples

``` r
withr::with_par(list("mfrow"=c(2,2)), {
for (alpha in c(1, 0.8, 0.5, 0.2)) {
   nullPlot(plotAreaTitle=paste0("alpha=", alpha),
      doMargins=FALSE);
   usrBox(fill=alpha2col("yellow",
      alpha=alpha));
}
})

```
