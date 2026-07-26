# Remove alpha transparency from colors

Remove alpha transparency from colors

## Usage

``` r
unalpha(x, keepNA = FALSE, ...)
```

## Arguments

- x:

  `character` vector of R colors

- keepNA:

  `logical` indicating whether `NA` values should be kept and therefore
  returned as `NA`. When `keepNA=FALSE` (default for backward
  compatibility) `NA` values are converted to `"#FFFFFF"` as done by
  [`grDevices::col2rgb()`](https://rdrr.io/r/grDevices/col2rgb.html).

- ...:

  additional arguments are ignored.

## Value

character vector of R colors in hex format.

## Details

This function simply removes the alpha transparency from R colors,
returned in hex format, for example `"#FF0000FF"` becomes `"#FF0000"`,
or `"blue"` becomes `"#0000FF"`.

It also silently converts R color names to hex format, where applicable.

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
[`warpRamp()`](https://jmw86069.github.io/jamba/reference/warpRamp.md)

## Examples

``` r
unalpha(c("#FFFF00DD", "red", NA, "#0000FF", "transparent"))
#> [1] "#FFFF00" "#FF0000" "#FFFFFF" "#0000FF" "#FFFFFF"

unalpha(c("#FFFF00DD", "red", NA, "#0000FF", "transparent"), keepNA=TRUE)
#> [1] "#FFFF00" "#FF0000" NA        "#0000FF" "#FFFFFF"
```
