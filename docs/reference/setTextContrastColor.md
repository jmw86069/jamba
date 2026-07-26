# Define visible text color

Given a vector or colors, define a contrasting color for text, typically
using either white or black. The `useGrey` argument defines the offset
from pure white and pure black, to use a contrasting grey shade.

## Usage

``` r
setTextContrastColor(
  color,
  hclCutoff = 60,
  rgbCutoff = 127,
  colorModel = c("hcl", "rgb"),
  useGrey = 0,
  keepAlpha = FALSE,
  alphaLens = 0,
  bg = NULL,
  ...
)
```

## Arguments

- color:

  character vector with one or more R-compatible colors.

- hclCutoff:

  numeric threshold above which a color is judged to be bright,
  therefore requiring a dark text color. This comparison uses the L
  value from the
  [`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md)
  function, which scales colors from 1 to 100.

- rgbCutoff:

  numeric threshold above which a color is judged to be bright,
  therefore requiring a dark text color. The mean r,g,b value is used.

- colorModel:

  Either 'hcl' or 'rgb' to indicate how the colors will be judged for
  overall brightness. The 'hcl' method uses the L value, which more
  reliably represents overall visible lightness.

- useGrey:

  numeric threshold used to define dark and bright text colors, using
  the R greyscale gradient from 0 to 100: `useGrey=10` implies
  `"grey10"` and `"grey90"` for the contrasting text colors;
  `useGrey=15` is useful if labels may also overlap white or black
  space, since the text will never be fully white or black.

- keepAlpha:

  logical indicates whether the input color alpha transparency should be
  maintained in the text color. By default, text alpha is not
  maintained, and instead is set to alpha=1, fully opaque.

- alphaLens:

  numeric value used to adjust the effect of alpha transparency, where
  positive values emphasize the background color, and negative values
  emphasize the foreground (transparent) color.

- bg:

  vector of R colors, used as a background when determining the
  brightness of a semi-transparent color. The corresponding brightness
  value from the `bg` is applied via weighted mean to the input `color`
  brightness, the result is compared the the relevant cutoff. By default
  `graphics::par("bg")` is used to determine the default plot background
  color, only when there is an open graphics device, otherwise calling
  `graphics::par("bg")` would open a graphics device, which is not
  desireable. When no graphics device is open, and when `bg=NULL`, the
  default is `bg="white"`.

- ...:

  additional arguments are ignored.

## Value

`character` vector of R colors.

## Details

The `color` is expected to represent a background color, the output is
intended to be a color with enough contrast to read text legibly.

The brightness of the `color` is detected dependent upon the
`colorModel`: when `"hcl"` the luminance `L` is compared to `hclCutoff`;
when `"rgb"` the brightness is the sum of the RGB channels which is
compared to `rgbCutoff`. In most cases the `"hcl"` and `L` will be more
accurate.

When `color` contains transparency, an optional argument `bg` represents
the figure background color, as if the `color` is used to draw a
color-filled rectangle. In this case, the `bg` and `color` are combined
to determine the resulting actual color. This scenario is mostly useful
when plotting text labels on a dark background, such as black background
with colored text boxes.

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
[`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md),
[`unalpha()`](https://jmw86069.github.io/jamba/reference/unalpha.md),
[`warpRamp()`](https://jmw86069.github.io/jamba/reference/warpRamp.md)

## Examples

``` r
color <- c("red","yellow","lightblue","darkorchid1","blue4");
setTextContrastColor(color);
#> [1] "#FFFFFFFF" "#000000FF" "#000000FF" "#FFFFFFFF" "#FFFFFFFF"

# showColors() uses setTextContrastColor() for labels
showColors(color)

# printDebugI() uses setTextContrastColor() for foreground text
printDebugI(color)
#> (16:01:37) 25Jul2026:    red,yellow,lightblue,darkorchid1,blue4

# demonstrate the effect of alpha transparency
colorL <- lapply(nameVector(c(1, 0.9, 0.8, 0.6, 0.3)), function(i){
   nameVector(alpha2col(color, alpha=i), color);
})
jamba::showColors(colorL,
   groupCellnotes=FALSE,
   srtCellnote=seq(from=15, to=-15, length.out=5));
graphics::title(ylab="alpha", line=1.5);


# change background to dark blue
withr::with_par(list("bg"="navy", "col"="white", "col.axis"="white"), {
jamba::showColors(colorL,
   groupCellnotes=FALSE,
   srtCellnote=seq(from=15, to=-15, length.out=5))
graphics::title(ylab="alpha", line=1.5);
})


# Example using transparency and drawLabels()
bg <- "blue4";
col <- fixYellow("palegoldenrod");
nullPlot(fill=bg, plotAreaTitle="", doMargins=FALSE);
for (alpha in c(0.1, 0.3, 0.5, 0.7, 0.9)) {
   labelCol <- setTextContrastColor(
      alpha2col("yellow", alpha),
      bg=bg);
   drawLabels(x=1 + alpha,
      y=2 - alpha,
      labelCex=1.5,
      txt="Plot Title",
      boxColor=alpha2col(col, alpha),
      boxBorderColor=labelCol,
      labelCol=labelCol);
}

```
