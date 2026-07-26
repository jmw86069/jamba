# Fix yellow color

Fix yellow color to be less green than default "yellow"

## Usage

``` r
fixYellow(col, Hrange = c(70, 100), Hshift = -20, fixup = TRUE, ...)
```

## Arguments

- col:

  R color, either in hex color format or using values from
  [`grDevices::colors()`](https://rdrr.io/r/grDevices/colors.html).

- Hrange:

  numeric vector whose range defines the region of hues to be adjusted.
  By default hues between 80 and 90 are adjusted. If NULL, `HCL` is
  return unchanged.

- Hshift:

  numeric value length one, used to adjust the hue of colors within the
  range `Hrange`. If NULL, `HCL` is return unchanged.

- fixup:

  `logical`, default TRUE, whether to apply fixup to the resulting
  color, passed to
  [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md)

- ...:

  additional arguments are passed to
  [`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md),
  and
  [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md).

## Value

returns a vector of R colors the same length as input `col`. In the
event `col`, `Hrange`, or `Hshift` have length 0, or if any step in the
conversion produces length 0, then the original `col` is returned.

## Details

This function "fixes" the color yellow, which by default appears green
especially when darkened. The effect of this function is to make yellows
appear more red, which appears more visibly yellow even when the color
is darkened.

This function is intended to be tolerant to missing values. For example
if any of the values `col`, `Hrange`, or `Hshift` are length 0, the
original `col` is returned unchanged.

## See also

Other jam color functions:
[`alpha2col()`](https://jmw86069.github.io/jamba/reference/alpha2col.md),
[`applyCLrange()`](https://jmw86069.github.io/jamba/reference/applyCLrange.md),
[`col2alpha()`](https://jmw86069.github.io/jamba/reference/col2alpha.md),
[`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md),
[`col2hsl()`](https://jmw86069.github.io/jamba/reference/col2hsl.md),
[`col2hsv()`](https://jmw86069.github.io/jamba/reference/col2hsv.md),
[`color2gradient()`](https://jmw86069.github.io/jamba/reference/color2gradient.md),
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
yellows <- vigrep("yellow", grDevices::colors());
fixedYellows <- fixYellow(yellows);
showColors(list(yellows=yellows,
   fixedYellows=fixedYellows));

```
