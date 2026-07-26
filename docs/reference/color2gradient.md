# Make a color gradient

Make a color gradient

## Usage

``` r
color2gradient(
  col,
  n = NULL,
  gradientWtFactor = NULL,
  dex = 1,
  reverseGradient = TRUE,
  verbose = FALSE,
  ...
)
```

## Arguments

- col:

  some type of recognized R color input as:

  - `character` vector of one or more individual colors, each color is
    expanded into a gradient of length `n`, where `n` is recycled to the
    number of unique colors. The value `n` is applied in the order the
    colors appear in `col`.

  - `list` of color vectors where each vector contains one repeated
    color

  - `character` vector of repeated colors, where `n` is defined by the
    number of each color present.

- n:

  `integer` vector of length one or more, which defines the number of
  colors to return for each gradient. When `n=0` then only duplicated
  colors will be expanded into a gradient.

- gradientWtFactor:

  `numeric` fraction representing the amount to expand a color toward
  its maximum brightness and darkness. It is recommended to use `dex`
  and not this argument.

  - When `gradientWtFactor=NULL` this value is calculated based upon the
    number of colors requested, and the initial luminance in HCL space
    of the starting color.

  - When `gradientWtFactor` is defined, values are recycled to
    `length(col)`, and can be independently applied to each color.

- dex:

  `numeric` value to apply dramatic dark expansion, where:

  - `dex > 1` will make the gradient more dramatic, values

  - `dex < 1` will make the gradient less dramatic, and are considered
    fractions 1/x.

  - `dex < 0` will make the gradient less dramatic, and values are
    internally converted to fractions using `1/(2 + abs(dex))`

- reverseGradient:

  `logical` whether to return light-to-dark gradient (TRUE) or
  dark-to-light gradient (FALSE).

- verbose:

  `logical` whether to print verbose output.

- ...:

  other parameters are ignored.

## Value

`character` vector of R colors.

## Details

This function converts a single color into a color gradient by expanding
the initial color into lighter and darker colors around the central
color. The amount of gradient expansion is controlled by
gradientWtFactor, which is a weight factor scaled to the maximum
available range of bright to dark colors.

As an extension, the function can take a vector of colors, and expand
each into its own color gradient, each with its own number of colors. If
a vector with supplied that contains repeated colors, these colors are
expanded in-place into a gradient, bypassing the value for `n`.

If a list is supplied, a list is returned of the same length, where each
vector inside the list is a color gradient of length specified by `n`.
If the input list contains multiple values, only the first color is used
to define the color gradient.

## See also

Other jam color functions:
[`alpha2col()`](https://jmw86069.github.io/jamba/reference/alpha2col.md),
[`applyCLrange()`](https://jmw86069.github.io/jamba/reference/applyCLrange.md),
[`col2alpha()`](https://jmw86069.github.io/jamba/reference/col2alpha.md),
[`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md),
[`col2hsl()`](https://jmw86069.github.io/jamba/reference/col2hsl.md),
[`col2hsv()`](https://jmw86069.github.io/jamba/reference/col2hsv.md),
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
# given a list, it returns a list
x <- color2gradient(list(Reds=c("red"), Blues=c("blue")), n=c(4,7));
showColors(x);


# given a vector, it returns a vector
xv <- color2gradient(c(red="red", blue="blue"), n=c(4,7));
showColors(xv);


# Expand colors in place
# This process is similar to color jittering
colors1 <- c("red","blue")[c(1,1,2,2,1,2,1,1)];
names(colors1) <- colors1;
colors2 <- color2gradient(colors1);
showColors(list(`Input colors`=colors1, `Output colors`=colors2));


# You can do the same using a list intermediate
colors1L <- split(colors1, colors1);
showColors(colors1L);

colors2L <- color2gradient(colors1L);
showColors(colors2L);


# comparison of fixed gradientWtFactor with dynamic gradientWtFactor
showColors(list(
   `dynamic\ngradientWtFactor\ndex=1`=color2gradient(
      c("yellow", "navy", "firebrick", "orange"),
      n=3,
      gradientWtFactor=NULL,
      dex=1),
   `dynamic\ngradientWtFactor\ndex=2`=color2gradient(
      c("yellow", "navy", "firebrick", "orange"),
      n=3,
      gradientWtFactor=NULL,
      dex=2),
   `fixed\ngradientWtFactor=2/3`=color2gradient(
      c("yellow", "navy", "firebrick", "orange"),
      n=3,
      gradientWtFactor=2/3,
      dex=1)
))

```
