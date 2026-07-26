# Show R function arguments jam-style

Show R function arguments jam-style

## Usage

``` r
jargs(
  x,
  grepString = NULL,
  sortVars = FALSE,
  useMessage = TRUE,
  asList = TRUE,
  useColor = TRUE,
  lightMode = getOption("jam.lightMode"),
  Crange = getOption("jam.Crange"),
  Lrange = getOption("jam.Lrange"),
  adjustRgb = getOption("jam.adjustRgb"),
  useCollapseBase = ", ",
  verbose = FALSE,
  debug = 0,
  ...
)
```

## Arguments

- x:

  `function` or character name of a function.

- grepString:

  'NULL', `logical`, or `character` grep regular expression pattern used
  to filter function arguments by name. Very useful to search a function
  for arguments with a substring `"row"`.

  - If `logical`, it is assumed to be sortVars, and indicates whether to
    sort the parameter names.

  - if `character` it will subset the function arguments by name
    matching this regular expression pattern.

- sortVars:

  `logical` whether to sort the function parameter names.

  - `sortVars=FALSE` returns arguments in the order they appear in the
    function definition.

  - `sortVars=TRUE` returns arguments sorted alphabetically.

- useMessage:

  `logical` default TRUE, whether to print output using
  [`message()`](https://rdrr.io/r/base/message.html), otherwise text is
  returned invisibly to be displayed separately.

- asList:

  `logical` default TRUE, display one entry per line or display results
  as a `data.frame`.

- useColor:

  `logical` whether to display results in color, if the crayon package
  is available, and terminal console is capable.

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

- useCollapseBase:

  `character` string used to combine multiple parameter values.

- verbose:

  `logical` whether to print verbose output.

- debug:

  `integer` value, greater than 0 will cause debug-type verbose output,
  useful because parameters are hard!

- ...:

  Additional arguments are installed.

## Value

'NULL' this function called for the byproduct of printing its output.

## Details

This function displays R function arguments, organized with one argument
per line, and colorized using the `crayon` package if installed.

Output is nicely spaced to help visual alignment of argument names and
argument values.

Output can be filtered by `character` pattern. For example the function
[`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html)
is amazing, and offers numerous arguments. To find arguments relevant to
dendrograms, use `"dend"`:

`jargs(ComplexHeatmap::Heatmap, "dend")`

NOTE: This function has edge case issues displaying complex function
argument values such as nested lists and custom functions. In that case
the argument name is printed as usual, and the argument value is
displayed as a partial snippet of the default argument value.

Generic functions very often contain no useful parameters, making it
difficult to discover required parameters without reading the function
documentation from the proper dispatched function and calling package.
In that case, try using `jargs(functionname.default)` for example
compare:

`jargs(barplot)`

to:

`jargs(barplot.default)`

## See also

Other jam practical functions:
[`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md),
[`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md),
[`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md),
[`check_pkg_installed()`](https://jmw86069.github.io/jamba/reference/check_pkg_installed.md),
[`colNum2excelName()`](https://jmw86069.github.io/jamba/reference/colNum2excelName.md),
[`color_dither()`](https://jmw86069.github.io/jamba/reference/color_dither.md),
[`exp2signed()`](https://jmw86069.github.io/jamba/reference/exp2signed.md),
[`getAxisLabel()`](https://jmw86069.github.io/jamba/reference/getAxisLabel.md),
[`isFALSEV()`](https://jmw86069.github.io/jamba/reference/isFALSEV.md),
[`isTRUEV()`](https://jmw86069.github.io/jamba/reference/isTRUEV.md),
[`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md),
[`lldf()`](https://jmw86069.github.io/jamba/reference/lldf.md),
[`log2signed()`](https://jmw86069.github.io/jamba/reference/log2signed.md),
[`middle()`](https://jmw86069.github.io/jamba/reference/middle.md),
[`minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md),
[`newestFile()`](https://jmw86069.github.io/jamba/reference/newestFile.md),
[`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md),
[`reload_qmd_cache()`](https://jmw86069.github.io/jamba/reference/reload_qmd_cache.md),
[`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md),
[`renameColumn()`](https://jmw86069.github.io/jamba/reference/renameColumn.md),
[`rmInfinite()`](https://jmw86069.github.io/jamba/reference/rmInfinite.md),
[`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md),
[`rmNAs()`](https://jmw86069.github.io/jamba/reference/rmNAs.md),
[`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md),
[`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)

## Examples

``` r
args(jargs)
#> function (x, grepString = NULL, sortVars = FALSE, useMessage = TRUE, 
#>     asList = TRUE, useColor = TRUE, lightMode = getOption("jam.lightMode"), 
#>     Crange = getOption("jam.Crange"), Lrange = getOption("jam.Lrange"), 
#>     adjustRgb = getOption("jam.adjustRgb"), useCollapseBase = ", ", 
#>     verbose = FALSE, debug = 0, ...) 
#> NULL
jargs(jargs)
#>               x = ,
#>      grepString = NULL,
#>        sortVars = FALSE,
#>      useMessage = TRUE,
#>          asList = TRUE,
#>        useColor = TRUE,
#>       lightMode = getOption("jam.lightMode"),
#>          Crange = getOption("jam.Crange"),
#>          Lrange = getOption("jam.Lrange"),
#>       adjustRgb = getOption("jam.adjustRgb"),
#> useCollapseBase = ", ",
#>         verbose = FALSE,
#>           debug = 0,
#>             ... = 

# retrieve parameters involving notes from imageByColors
jargs(imageByColors, "note")
#>       cellnote = NULL,
#>    cexCellnote = 1,
#>    srtCellnote = 0,
#>   fontCellnote = 1,
#> groupCellnotes = TRUE
```
