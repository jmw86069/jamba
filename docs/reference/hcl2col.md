# convert HCL to R color

Convert an HCL color matrix to vector of R hex colors

## Usage

``` r
hcl2col(
  x = NULL,
  H = NULL,
  C = NULL,
  L = NULL,
  ceiling = 255,
  maxColorValue = 255,
  alpha = NULL,
  fixup = TRUE,
  model = getOption("jam.model", c("hcl", "polarLUV", "polarLAB")),
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  matrix of colors, with rownames `"H"`, `"C"`, `"L"`, or if not
  supplied it looks for vectors `H`, `C`, and `L` accordingly. It can
  alternatively be supplied as an object of class `polarLUV`.

- H, C, L:

  numeric vectors supplied as an alternative to `x`, with ranges 0 to
  360, 0 to 100, and 0 to 100, respectively.

- ceiling:

  numeric value indicating the maximum values allowed for `R`, `G`, and
  `B` after conversion by `colorspace::as(x, "RGB")`. This ceiling is
  applied after the `maxColorValue` is used to scale numeric values, and
  is intended to correct for the occurrence of values above 255, which
  would be outside the typical color gamut allowed for RGB colors used
  in R. In general, this value should not be modified.

- maxColorValue:

  numeric value indicating the maximum RGB values, typically scaling
  values to a range of 0 to 255, from the default returned range of 0
  to 1. In general, this value should not be modified.

- alpha:

  optional vector of alpha values. If not supplied, and if `x` is
  supplied as a matrix with rowname `"alpha"`, then values will be used
  from `x["alpha",]`.

- fixup:

  boolean indicating whether to use `colorspace::hex(...,fixup=TRUE)`
  for conversion to R hex colors, **which is not recommended** since
  this conversion applies some unknown non-linear transformation for
  colors outside the color gamut. It is here is an option for
  comparison, and if specifically needed.

- model:

  `character` string indicating the color model to use:

  - hcl (default) uses `farver`

  - polarLUV uses `colorspace` polarLUV

  - polarLAB uses \`colorspace polarLAB

- verbose:

  `logical` whether to print verbose output.

- ...:

  other arguments are ignored.

## Value

vector of R colors, or where the input was NA, then NA values are
returned in the same order.

## Details

This function takes an HCL matrix,and converts to an R color using the
colorspace package
[`colorspace::polarLUV()`](https://colorspace.R-Forge.R-project.org/reference/polarLUV.html)
and
[`colorspace::hex()`](https://colorspace.R-Forge.R-project.org/reference/hex.html).

When `model="hcl"` this function uses
[`farver::encode_colour()`](https://farver.data-imaginist.com/reference/encode_colour.html)
and bypasses `colorspace`. In future the `colorspace` dependency will
likely be removed in favor of using `farver`. In any event,
`model="hcl"` is equivalent to using `model="polarLUV"` and
`fixup=TRUE`, except that it should be much faster.

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
# Prepare a basic HCL matrix
hclM <- col2hcl(c(red="red",
   blue="blue",
   yellow="yellow",
   orange="#FFAA0066"));
hclM;
#>             red      blue    yellow    orange
#> H      12.17440 265.87459  85.86596  46.97366
#> C     179.04898 130.67920 107.07044 103.64939
#> L      53.24079  32.29701  97.13927  76.07836
#> alpha   1.00000   1.00000   1.00000   0.40000

# Now convert back to R hex colors
colorV <- hcl2col(hclM);
colorV;
#>       red      blue    yellow    orange 
#> "#FF0000" "#0000FF" "#FFFF00" "#FFAA00" 

showColors(colorV);

```
