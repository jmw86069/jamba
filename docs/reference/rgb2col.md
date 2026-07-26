# Convert RGB color matrix to R color

Convert RGB color matrix to R color

## Usage

``` r
rgb2col(
  red,
  green = NULL,
  blue = NULL,
  alpha = NULL,
  names = NULL,
  maxColorValue = NULL,
  keepNA = TRUE,
  verbose = FALSE,
  ...
)
```

## Arguments

- red:

  `numeric` vector of red values; or RGB numeric matrix with rownames
  c("red","green","blue") in any order, with optional rowname "alpha";
  or character strings with comma-separated rgb values, in format
  "100,20,10". The latter input is designed to handle web rgb values.

- green:

  `numeric` vector, or when red is a matrix or comma-delimited character
  string, this parameter is ignored.

- blue:

  `numeric` vector, or when red is a matrix or comma-delimited character
  string, this parameter is ignored.

- alpha:

  `numeric` vector, or when red is a matrix or comma-delimited character
  string, this parameter is ignored. Alpha values are always expected in
  range `[0,1]`, even when `maxColorValue` is higher than `1`. When
  `alpha` is `FALSE`, the alpha transparency is removed. When `alpha` is
  `TRUE` the original alpha transparency is retained without change. If
  supplying `alpha` as a numeric vector, use `Inf` to represent `TRUE`
  for alpha values to be kept without change, and use `-1` or any
  negative number to indicate alpha values to remove from the output.

- names:

  `character`, default NULL, with optional names to apply to output
  colors.

- maxColorValue:

  `numeric` maximum value for colors. If NULL then it defaults to 1
  unless there are values above 1, in which case it defaults to 255.

- keepNA:

  `logical` whether to keep NA values, returning NA for any input where
  red, green, and/or blue are NA. If keepNA==FALSE then it substitutes 0
  for any NA values.

- verbose:

  `logical` indicating whether to print verbose output

- ...:

  Additional arguments are ignored.

## Value

`character` vector of R colors.

## Details

This function intends to augment the
[`rgb`](https://rdrr.io/r/grDevices/rgb.html) function, which does not
handle output from
[`col2rgb`](https://rdrr.io/r/grDevices/col2rgb.html). The goal is to
handle multiple color conversions, e.g.
`rgb2col(grDevices::col2rgb("red"))`. This function also maintains alpha
transparency when supplied.

The output is named either by names(red), rownames(red), or if supplied,
the value of the parameter `names`.

Note that `alpha` is used to define alpha transparency, but has
additional control over the output.

- When `alpha` is `FALSE` then output colors will not have the alpha
  transparency, in hex form that means colors are in format `"#RRGGBB"`
  and not `"#RRGGBBAA"`.

- When `alpha` is `TRUE` the previous alpha transparency values are used
  without change.

- When `alpha` is a numeric vector, numeric values are always expected
  to be in range `[0,1]`, where `0` is completely transparent, and `1`
  is completely not transparent. Supplied `alpha` values will override
  those present in `red` when `red` is a matrix like that produced from
  `grDevices::col2rgb(..., alpha=TRUE)`.

- When `alpha` is a numeric vector, use `-1` or any negative number to
  indicate the alpha value should be removed.

- When `alpha` is a numeric vector, use `Inf` to indicate the alpha
  transparency should be retained without change.

Therefore, `alpha = c(-1, 0, 1, Inf)` will apply the following, in
order: remove alpha; set alpha to 0; set alpha to 1; set alpha to the
same as the input color.

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
[`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md),
[`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md),
[`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md),
[`unalpha()`](https://jmw86069.github.io/jamba/reference/unalpha.md),
[`warpRamp()`](https://jmw86069.github.io/jamba/reference/warpRamp.md)

## Examples

``` r
# start with a color vector
# red and blue with partial transparency
colorV <- c("#FF000055", "#00339999");

# Show the output of rgb2col
# make sure to include alpha=TRUE to maintain alpha transparency
grDevices::col2rgb(colorV, alpha=TRUE);
#>       [,1] [,2]
#> red    255    0
#> green    0   51
#> blue     0  153
#> alpha   85  153

# confirm we can convert from RGB back to the same colors
rgb2col(grDevices::col2rgb(colorV, alpha=TRUE));
#> [1] "#FF000055" "#00339999"
```
