# Get Chroma and Luminance ranges for the given lightMode

Return Crange, Lrange, Cgrey, adjustRgb values for the given lightMode,
intended to provide ranges suitable for contrasting text displayed on a
light or dark background.

## Usage

``` r
setCLranges(
  lightMode = NULL,
  Crange = getOption("jam.Crange"),
  Lrange = getOption("jam.Lrange"),
  Cgrey = getOption("jam.Cgrey", 5),
  adjustRgb = getOption("jam.adjustRgb", 0),
  setOptions = c("FALSE", "ifnull", "TRUE"),
  verbose = FALSE,
  ...
)
```

## Arguments

- lightMode:

  `logical` indicating whether the background color is light (TRUE is
  bright), or dark (FALSE is dark.)

  - When TRUE or FALSE, it will set default values for Crange and
    Lrange.

  - When NULL, it will use one or both of Crange,Lrange when supplied,
    and uses `getOption("jam.Crange")` and `getOption("jam.Lrange")` by
    default when not explicitly provided.

  - If lightMode is NULL and one or both of Crange, Lrange are NULL, it
    calls
    [`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md)
    then assigns appropriate default values.

  - To detect lightMode again, use `lightMode=checkLightMode()`.

- Crange:

  `numeric` range of chroma values, ranging between 0 and 100. By
  default, `getOptions("Crange")` is used, otherwise defaults will be
  assigned based upon `lightMode`.

- Lrange:

  `numeric` range of luminance values, ranging between 0 and 100. By
  default, `getOptions("Crange")` is used, otherwise defaults will be
  assigned based upon `lightMode`.

- Cgrey:

  `numeric` chroma (C) value, which defines grey colors at or below this
  chroma. Any colors at or below the grey cutoff will have their C
  values unchanged. This threshold prevents colorizing greyscale colors
  via Crange. To disable the effect, set `Cgrey=-1`.

- adjustRgb:

  `numeric` color adjustment factor, used during the conversion of RGB
  colors to the ANSI-compatible colors used by the `crayon` pacakge. The
  ANSI color range does not include a full RGB palette, and the
  conversion is somewhat lossy. By default,
  `getOptions("jam.adjustRgb")` is used to store a globally re-usable
  value.

- setOptions:

  `character` or `logical` whether to update
  [`options()`](https://rdrr.io/r/base/options.html) `"jam.Crange"` and
  `"jam.Lrange"`, with default 'FALSE'. It has the following behavior:

  - `FALSE` or `"FALSE"` does not update options.

  - `TRUE` or `"TRUE"` will update options 'jam.Crange' and
    'jam.Lrange'.

  - `"ifnull"` will update only
    [`options()`](https://rdrr.io/r/base/options.html) which were
    previously blank.

- verbose:

  `logical` indicating whether to print verbose output.

- ...:

  additional arguments are ignored.

## Value

`list`, invisibly, with elements:

- Crange:

  Numeric vector of length 2, defining the HCL chroma (C) range.

- Lrange:

  Numeric vector of length 2, defining the HCL luminance (L) range.

- adjustRgb:

  Numeric vector of length 1, defining the adjustment to apply during
  RGB-to-ANSI color conversion.

- Cgrey:

  Numeric vector of length 1, defining the HCL chroma (C) value below
  which colors are considered greyscale, and are converted to ANSI
  greyscale colors. HCL chroma ranges from 0 to 100. Set value
  `Cgrey=-1` or `Cgrey=FALSE` to disable this logic, causing colors to
  be matched using all available ANSI color values.

## Details

This function is intended mainly for internal use by `jamba` such as
[`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md),
and
[`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md),
which is also mainly intended for console text or other printed text
output. The utility of this function is to store the logic of
determining sensible default ranges.

Companion functions:

- `applyCLranges()` is used to apply the ranges to a vector of R colors.

- [`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md)
  is used to detect whether console output is expected to have a light
  or dark background.

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
[`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md),
[`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md),
[`unalpha()`](https://jmw86069.github.io/jamba/reference/unalpha.md),
[`warpRamp()`](https://jmw86069.github.io/jamba/reference/warpRamp.md)

## Examples

``` r
setCLranges(lightMode=FALSE)
```
