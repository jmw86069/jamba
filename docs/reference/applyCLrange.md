# Apply CL color range

Restrict chroma (C) and luminance (L) ranges for a vector of R colors

## Usage

``` r
applyCLrange(
  x,
  lightMode = NULL,
  Crange = getOption("jam.Crange"),
  Lrange = getOption("jam.Lrange"),
  Cgrey = getOption("jam.Cgrey", 5),
  fixYellow = TRUE,
  CLmethod = c("scale", "floor", "expand"),
  fixup = TRUE,
  ...
)
```

## Arguments

- x:

  vector of R colors

- lightMode:

  'NULL' or `logical`. When `lightMode=NULL` then `Crange` and `Lrange`
  values are used as-is; when `lightMode=TRUE` or `lightMode=FALSE` then
  default values are used for `Crange` and `Lrange` values, where
  `lightMode=TRUE` is intended for colors to have contrast against a
  light/bright/white background, and `lightMode=FALSE` is intended for
  colors to have contrast against a dark background.

- Crange:

  'NULL' or `numeric` range with minimum and maximum allowed values for
  the chroma (C) component.

- Lrange:

  `NUL`L or `numeric` range with minimum and maximum allowed values for
  the luminance (L) component.

- Cgrey:

  `numeric` chroma (C) value, which defines grey colors at or below this
  chroma. Any colors at or below the grey cutoff will have their C
  values unchanged. This mechanism prevents converting black to red, for
  example. To disable the effect, set `Cgrey=-1`.

- fixYellow:

  `logical` indicating whether to "fix" the darkening of yellow, which
  otherwise turns to green. Instead, since JAM can, JAM will make the
  yellow slightly more golden before darkening, which is achieved by
  calling
  [`fixYellowHue()`](https://jmw86069.github.io/jamba/reference/fixYellowHue.md).

- CLmethod:

  `character` string indicating how to alter values outside the
  respective `Crange` and `Lrange` ranges. "scale" will rescale values
  only if any are outside of range, and will rescale the full range of
  `c(Crange, Cvalues)` to `c(Crange)`. In this way, only values outside
  the range are rescaled. "floor" will apply a fixed cutoff, any values
  outside the range are set to equal the range boundary itself. "expand"
  will rescale all values so the range is equal to `Crange`.

- fixup:

  `logical` passed to
  [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md)
  and subsequently to
  [`colorspace::hex()`](https://colorspace.R-Forge.R-project.org/reference/hex.html)
  when converting colors outside the color gamut (visible range.) When
  `fixup` is 'NULL', the
  [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md)
  method applies its own aggressive technique to restrict the color
  range.

- ...:

  additional argyments are passed to
  [`fixYellowHue()`](https://jmw86069.github.io/jamba/reference/fixYellowHue.md)
  when `fixYellow` is `TRUE`.

## Value

vector of colors after applying the chroma (C) and luminance (L) ranges.

## Details

This function is primarily intended to restrict the range of brightness
values so they contrast with a background color, particularly when the
background color may be bright or dark.

Note that output is slightly different when supplying one color,
compared to supplying a vector of colors. One color is simply restricted
to the `Crange` and `Lrange`. However, a vector of colors is scaled
within the ranges so that relative `C` and `L` values are maintained,
for visual comparison.

The C and L values are defined by
[`colorspace::polarLUV()`](https://colorspace.R-Forge.R-project.org/reference/polarLUV.html),
where C is typically restricted to `0..100` and L is typically `0..100`.
For some colors, values above 100 are allowed.

Values are restricted to the given numeric range using one of three
methods, set via the `CLmethod` argument.

As an example, consider what should be done when `Crange <- c(10,70)`
and the C values are `Cvalues <- c(50, 60, 70, 80)`.

1.  "floor" uses
    [`jamba::noiseFloor()`](https://jmw86069.github.io/jamba/reference/noiseFloor.md)
    to apply fixed cutoffs at the minimum and maximum range. This method
    has the effect of making all values outside the range into an equal
    final value.

2.  "scale" will apply
    [`jamba::normScale()`](https://jmw86069.github.io/jamba/reference/normScale.md)
    to rescale only values outside the given range. For example,
    `c(Crange, Cvalues)` as the initial range, it constrains values to
    `c(Crange)`. This method has the effect of maintaining the relative
    difference between values.

3.  "expand" will simply apply
    [`jamba::normScale()`](https://jmw86069.github.io/jamba/reference/normScale.md)
    to fit the values to the minimum and maximum range values. This
    method has the effect of forcing colors to fit the full numeric
    range, even when the original differences between values were small.

In case (1) above, Cvalues will become `c(50, 60, 70, 70)`. In case (2)
above, Cvalues will become `c(44, 53, 61, 70)` In case (3) above,
Cvalues will become `c(10, 30, 50, 70)`

Note that colors with C (chroma) values less than `Cgrey` will not have
the C value changed, in order to maintain colors at a greyscale, without
colorizing them. Particularly for pure `grey`, which has `C=0`, but is
still required to have a hue H, it is important not to increase `C`.

## See also

Other jam color functions:
[`alpha2col()`](https://jmw86069.github.io/jamba/reference/alpha2col.md),
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
cl <- c("red", "blue", "navy", "yellow", "orange");
cl_lite <- applyCLrange(cl, lightMode=TRUE);
cl_dark <- applyCLrange(cl, lightMode=FALSE);

# individual colors
cl_lite_ind <- sapply(cl, applyCLrange, lightMode=TRUE);
cl_dark_ind <- sapply(cl, applyCLrange, lightMode=FALSE);

# display colors
showColors(list(`input colors`=cl,
   `lightMode=TRUE, vector`=cl_lite,
   `lightMode=TRUE, individual`=cl_lite_ind,
   `lightMode=FALSE, vector`=cl_dark,
   `lightMode=FALSE, individual`=cl_dark_ind))

printDebug(cl, lightMode=TRUE);
#> (15:59:57) 25Jul2026:    red,blue,navy,yellow,orange
```
