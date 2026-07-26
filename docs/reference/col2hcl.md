# convert R color to HCL color matrix

convert R color to HCL color matrix

## Usage

``` r
col2hcl(
  x,
  maxColorValue = 255,
  model = getOption("jam.model", c("hcl", "polarLUV", "polarLAB")),
  ...
)
```

## Arguments

- x:

  `character` R compatible color, either a color name, hex value, or a
  mixture of the two. Any value compatible with
  [`grDevices::col2rgb()`](https://rdrr.io/r/grDevices/col2rgb.html).

- maxColorValue:

  `numeric` maximum value to return, useful when the downstream alpha
  range should be 255. By default maxValue=1 is returned.

- model:

  `character` color model to use

  - `"hcl"` to use `farver` HCL

  - `"polarLUV"` for the standard R conventional HCL,

  - `"polarLAB"` which uses the LAB-based HCL values.

- ...:

  additional arguments are ignored.

## Value

`numeric` matrix with H, C, L values.

## Details

This function takes an R color and converts to an HCL matrix, using the
colorspace package, and
[`RGB`](https://colorspace.R-Forge.R-project.org/reference/RGB.html) and
[`polarLUV`](https://colorspace.R-Forge.R-project.org/reference/polarLUV.html)
functions. It is also used to maintain alpha transparency, to enable
interconversion via other color manipulation functions as well.

When `model="hcl"` this function uses
[`farver::decode_colour()`](https://farver.data-imaginist.com/reference/decode_colour.html)
and bypasses `colorspace`. In future the `colorspace` dependency will
likely be removed in favor of using `farver`. In any event,
`model="hcl"` is equivalent to using `model="polarLUV"` and
`fixup=TRUE`, except that it should be much faster.

## See also

Other jam color functions:
[`alpha2col()`](https://jmw86069.github.io/jamba/reference/alpha2col.md),
[`applyCLrange()`](https://jmw86069.github.io/jamba/reference/applyCLrange.md),
[`col2alpha()`](https://jmw86069.github.io/jamba/reference/col2alpha.md),
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
col2hcl("#FF000044")
#>              [,1]
#> H      12.1743993
#> C     179.0489832
#> L      53.2407941
#> alpha   0.2666667
```
