# convert HCL to R color

Convert an HCL color matrix to vector of R hex colors

## Usage

``` r
hsl2col(
  x = NULL,
  H = NULL,
  S = NULL,
  L = NULL,
  alpha = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  `numeric` matrix of colors, with rownames `"H"`, `"S"`, `"L"`, or if
  not supplied it looks for vectors `H`, `S`, and `L` accordingly.

- H, S, L:

  `numeric` vectors supplied as an alternative to `x`, with ranges 0 to
  360, 0 to 100, and 0 to 100, respectively.

- alpha:

  `numeric` vector of alpha values, default NULL. If not supplied, and
  if `x` is supplied as a matrix with rowname `"alpha"`, then values
  will be used from `x["alpha",]`.

- verbose:

  `logical` indicating whether to print verbose output.

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
[`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md),
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
# See col2hcl() for more extensive examples

# Prepare a basic HSL matrix
x_colors <- c(red="red",
   blue="blue",
   yellow="yellow",
   orange="#FFAA0066");
hslM <- col2hsl(x_colors);
hslM;
#>       red blue yellow orange
#> H       0  240     60   40.0
#> S     100  100    100  100.0
#> L      50   50     50   50.0
#> alpha   1    1      1    0.4

# Now convert back to R hex colors
colorV <- hsl2col(hslM);
colorV;
#>         red        blue      yellow      orange 
#>   "#FF0000"   "#0000FF"   "#FFFF00" "#FFAA0066" 

showColors(list(x_colors=x_colors,
   colorV=nameVector(colorV)));

```
