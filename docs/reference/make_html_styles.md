# vectorized make_styles for html span output

vectorized make_styles for html span output

## Usage

``` r
make_html_styles(
  style = NULL,
  text,
  bg = FALSE,
  bg_style = NULL,
  grey = FALSE,
  Cgrey = getOption("jam.Cgrey"),
  lightMode = NULL,
  Crange = getOption("jam.Crange"),
  Lrange = getOption("jam.Lrange"),
  adjustRgb = getOption("jam.adjustRgb"),
  adjustPower = 1.5,
  fixYellow = TRUE,
  alphaPower = 2,
  setOptions = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- style:

  `character` vector of one or more styles. When 'NULL' or `NA`, no
  style is applied, except when `bg_style` is supplied and is neither
  `NA` nor 'NULL', in which case entries with a `bg_style` and no
  `style` will use
  [`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md)
  to define a contrasting `style`.

- text:

  `character` vector (or coerced to `character`) of one or more values,.

- bg:

  `logical` indicating whether the `style` should be applied to the
  background instead of foreground. This argument is ignored when
  `bg_style` is supplied.

- bg_style:

  'NULL' or a `character` vector of one or more background styles. When
  this argument is not NULL, it applies both the foreground `style` and
  background `bg_style` together, and therefore ignores `Crange` and
  `Lrange` settings.

- grey:

  `logical`, default FALSE, whether to use greyscale.

- Cgrey:

  `numeric` chroma (C) value, which defines grey colors at or below this
  chroma. Any colors at or below the grey cutoff will have use ANSI
  greyscale coloring. To disable, set `Cgrey=-1`.

- lightMode:

  `logical` indicating whether the background color is light (TRUE is
  bright), or dark (FALSE is dark.) By default it calls
  [`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md)
  which queries `getOption("lightMode")`.

- Crange:

  `numeric` range of chroma values, ranging between 0 and 100. When
  NULL, default values will be assigned to Crange. When supplied,
  range(Crange) is used.

- Lrange:

  `numeric` range of luminance values, ranging between 0 and 100. When
  NULL, default values will be assigned to Lrange. When supplied,
  range(Lrange) is used.

- adjustRgb:

  `numeric` value adjustment used during the conversion of RGB colors to
  ANSI colors, which is inherently lossy. If not defined, it uses the
  default returned by
  [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md)
  which itself uses `getOption("jam.adjustRgb")` with default=0. In
  order to boost color contrast, an alternate value of -0.1 is
  suggested.

- adjustPower:

  `numeric` adjustment power factor

- fixYellow:

  `logical` indicating whether to "fix" the darkening of yellow, which
  otherwise turns to green. Instead, since JAM can, JAM will make the
  yellow slightly more golden before darkening. This change only affects
  color hues between 80 and 90. This argument is passed to
  [`applyCLrange()`](https://jmw86069.github.io/jamba/reference/applyCLrange.md).

- alphaPower:

  `numeric` value, used to adjust the RGB values for alpha values less
  than 255, by raising the ratio to 1/alphaPower, which takes the ratio
  of square roots. alphaPower=100 for minimal adjustment.

- setOptions:

  `character` or `logical` whether to update `Crange` and `Lrange`
  options during the subsequent call to
  [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md).
  By default,

  - `"ifnull"` will update only options which were previously 'NULL';

  - `"FALSE"` prevents modifying the global options;

  - `"TRUE"` will update these options with the current values.

- verbose:

  `logical` indicating whether to print verbose output

- ...:

  additional parameters are ignored

## Value

`character` vector with the same length as `text` input vector, where
entries are surrounded by the relevant HTML consistent with the `style`
defined at input. In short, a character vector as input, colorized HTML
character vector as output.

## Details

Note this function is experimental.

## See also

Other jam internal functions:
[`handleArgsText()`](https://jmw86069.github.io/jamba/reference/handleArgsText.md),
[`jamCalcDensity()`](https://jmw86069.github.io/jamba/reference/jamCalcDensity.md),
[`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md),
[`smoothScatterJam()`](https://jmw86069.github.io/jamba/reference/smoothScatterJam.md)

## Examples

``` r
make_html_styles(style=c("red", "orange"), text=c("one ", "two"))
#> [1] "<span style=\"color:#F40000\">one </span>"
#> [2] "<span style=\"color:#F09800\">two</span>" 
```
