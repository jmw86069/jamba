# Handle function arguments as text

Handles a list or list of lists, converting to human-readable text
format

## Usage

``` r
handleArgsText(
  argTextA,
  name = "",
  col1 = "mediumpurple2",
  col2 = "mediumaquamarine",
  colT = "dodgerblue3",
  colF = "red1",
  colNULL = "grey60",
  lightMode = NULL,
  Crange = getOption("jam.Crange"),
  Lrange = getOption("jam.Lrange"),
  adjustRgb = getOption("jam.adjustRgb"),
  indent = "",
  useCollapseList = ",\n      ",
  useCollapseBase = ", ",
  level = 1,
  debug = 0,
  useColor = TRUE,
  verbose = FALSE,
  ...
)
```

## Arguments

- argTextA:

  object passed by
  [`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md) when
  iteratively parsing function argument values.

- name:

  `character` name of the argument.

- col1, col2, colT, colF, colNULL:

  `character` colors used as defaults for first and second arguments,
  TRUE, FALSE, NULL, respectively.

- lightMode:

  `logical` or 'NULL', indicating whether the text background color is
  light, thus imposing a maximum brightness for colors displayed. It use
  lightMode if defined by the function caller, otherwise it will use
  `getOption("jam.lightMode")` if defined, lastly it will attempt to
  detect whether running inside Rstudio by checking the environment
  variable "RSTUDIO", and if so it will assume lightMode==TRUE.

- Crange:

  `numeric` range of chroma values, ranging between 0 and 100. When
  NULL, default values will be assigned to Crange by
  [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md).

- Lrange:

  `numeric` range of luminance values, ranging between 0 and 100. When
  NULL, default values will be assigned to Lrange by
  [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md).

- adjustRgb:

  `numeric` value adjustment used during the conversion of RGB colors to
  ANSI colors, which is inherently lossy. If not defined, it uses the
  default returned by
  [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md)
  which itself uses `getOption("jam.adjustRgb")` with default=0. In
  order to boost color contrast, an alternate value of -0.1 is
  suggested.

- indent:

  `character` string used as a prefix in output to help apply text
  indent.

- useCollapseList:

  `character` string inserted between multiple values to split list
  entries across multiple lines.

- useCollapseBase:

  `character` string used to separate multiple values in a vector which
  is not split across multiple lines.

- level:

  `integer` indicating the level of depth in iterative parsing.

- debug:

  `integer` value, greater than 0 will cause debug-type verbose output,
  useful because parameters are hard!

- useColor:

  `logical` whether to display results in color, if the crayon package
  is available, and terminal console is capable.

- verbose:

  `logical` whether to print verbose output.

- ...:

  Additional arguments are ignored.

## Value

`character` vector including ANSI coloring when available.

## Details

This function is a rare non-exported function intended to be called by
[`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md), but
separated in order to help isolate the logical steps required.

## See also

Other jam internal functions:
[`jamCalcDensity()`](https://jmw86069.github.io/jamba/reference/jamCalcDensity.md),
[`make_html_styles()`](https://jmw86069.github.io/jamba/reference/make_html_styles.md),
[`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md),
[`smoothScatterJam()`](https://jmw86069.github.io/jamba/reference/smoothScatterJam.md)

## Examples

``` r
cat(paste0(handleArgsText(formals(graphics::hist.default)), "\n"), sep="")
#> x
#> breaks="Sturges"
#> freq=NULL
#> probability=!freq
#> include.lowest=TRUE
#> right=TRUE
#> fuzz=1e-07
#> density=NULL
#> angle=45
#> col="lightgray"
#> border=NULL
#> main=paste("Histogram of", xname)
#> xlim=range(breaks)
#> ylim=NULL
#> xlab=xname
#> ylab
#> axes=TRUE
#> plot=TRUE
#> labels=FALSE
#> nclass=NULL
#> warn.unused=TRUE
#> ...
```
