# Fix yellow color hue

Fix yellow color hue to be less green than default "yellow"

## Usage

``` r
fixYellowHue(HCL, Hrange = c(80, 90), Hshift = -15, ...)
```

## Arguments

- HCL:

  numeric matrix with HCL color values, as returned by
  [`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md),
  but requiring only one rowname `"H"` representing the color hue on a
  scale of 0 to 360. If input data does not contain numeric values with
  rowname "H", `HCL` is return unchanged.

- Hrange:

  numeric vector whose range defines the region of hues to be adjusted.
  By default hues between 80 and 90 are adjusted. If NULL, `HCL` is
  return unchanged.

- Hshift:

  numeric value length one, used to adjust the hue of colors within the
  range `Hrange`. If NULL, `HCL` is return unchanged.

- ...:

  additional arguments are ignored.

## Value

returns the input `HCL` data where rowname `"H"` has hue values adjusted
accordingly. In the event `HCL`, `Hrange`, or `Hshift` have length 0,
the original `HCL` is returned. If input data does not meet the expected
format, the input `HCL` is returned unchanged.

## Details

This function "fixes" the color yellow, which by default appears green
especially when darkened. The effect of this function is to make yellows
appear more red, which appears more visibly yellow even when the color
is darkened.

This function is intended to be tolerant to missing values. For example
if any of the values `HCL`, `Hrange`, or `Hshift` are length 0, the
original `HCL` is returned unchanged.

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
yellowsHCL <- col2hcl(yellows);
fixedYellowsHCL <- fixYellowHue(yellowsHCL);
fixedYellows <- hcl2col(fixedYellowsHCL);
showColors(list(yellows=yellows,
   fixedYellows=fixedYellows));

```
