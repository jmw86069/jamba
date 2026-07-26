# Print colorized output to R console

Print colorized output to R console using R messages, with convenient
time stamp, for debug type output.

print colorized output to R console, inverted

print colorized output to HTML

## Usage

``` r
printDebug(
  ...,
  fgText = NULL,
  fgDefault = getOption("jam.fgDefault", c("darkorange1", "dodgerblue")),
  bgText = NULL,
  fgTime = getOption("jam.fgTime", "cyan2"),
  timeStamp = getOption("jam.timeStamp", TRUE),
  formatNumbers = getOption("jam.formatNumbers", TRUE),
  trim = getOption("jam.trim", TRUE),
  digits = getOption("jam.digits"),
  nsmall = getOption("jam.nsmall", 0L),
  justify = "left",
  big.mark = getOption("jam.big.mark", ","),
  small.mark = getOption("jam.small.mark", "."),
  zero.print = NULL,
  width = NULL,
  doColor = getOption("jam.doColor"),
  splitComments = FALSE,
  collapse = getOption("jam.collapse", ""),
  sep = getOption("jam.sep", ","),
  doReset = NULL,
  detectColors = TRUE,
  dex = 2,
  darkFactor = c(1, 1.5),
  sFactor = c(1, 1.5),
  lightMode = NULL,
  Crange = getOption("jam.Crange"),
  Lrange = getOption("jam.Lrange"),
  removeNA = FALSE,
  replaceNULL = NULL,
  adjustRgb = getOption("jam.adjustRgb"),
  byLine = FALSE,
  verbose = FALSE,
  indent = "",
  keepNA = TRUE,
  file = getOption("jam.file", ""),
  append = getOption("jam.append", TRUE),
  invert = getOption("jam.invert", FALSE),
  htmlOut = getOption("jam.htmlOut"),
  comment = getOption("jam.comment")
)

printDebugI(..., invert = TRUE)

printDebugHtml(..., htmlOut = TRUE, comment = FALSE)
```

## Arguments

- ...:

  `character`, `factor`, `numeric` or compatible atomic vectors to be
  printed to the R console. These arguments are recognized as any
  un-named argument, or any argument whose name does not match the named
  arguments below.

- fgText:

  one of two formats to define the foreground color for elements in
  `...` being printed. Each element is colored in order, and when
  multiple vector values are contained in one `...` element, the color
  defined in `fgText` is extended. The input types recognized:

  - 'NULL' when no color is defined, one of two outputs:

    1.  When all values in `...` represent colors, these colors are used
        to colorize the output text. When
        [`names()`](https://rdrr.io/r/base/names.html) are present they
        are used as the text labels in place of the vector value.

    2.  When not all values in `...` represent colors, the default color
        set is used: `c("darkorange1", "dodgerblue")`.

    3.  To disable option 1 above, define a specific value for `fgText`,
        such as `fgText=c("darkorange1", "dodgerblue")`.

  - `vector` of R compatible colors, recycled to the length of `...`.
    When any element of `...` is a vector with multiple values, the
    corresponding color in `fgText` is shaded slightly lighter and
    darker, then recycled to the vector length, so that adjacent values
    have slightly different color. This behavior is controlled by
    default argument `splitComments=TRUE`.

  - `list` of vectors of R compatible colors, recycled to the length of
    `...`, then applied to each element in `...` in order. When only one
    color is defined, and multiple values are present in the
    corresponding `list` element, the color is shaded slightly lighter
    and darker, then recycled to the vector length, as described above.
    This behavior is controlled by default argument
    `splitComments=TRUE`. When multiple colors are defined for the
    `list` element, these values are recycled to the vector length.

  - **Note**: When `invert=TRUE` the values for `fgText` and `bgText`
    are reversed, and if the resulting `fgText` is 'NULL' then its color
    is defined by
    [`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md)
    in order to define a contrasting text color.

- fgDefault:

  `character` defaults to
  `getOption("jam.fgDefault", c("darkorange1", "dodgerblue"))`, and is
  used when colors are not defined by `fgText` or by the input `...`
  values.

- bgText:

  `vector` of R colors, or `list` of vectors, used to define the
  background color, using the same approach described for `fgText`. Note
  that 'NULL' or `NA` defines the absence of any background color, which
  is default. When `invert=TRUE`, which is default for `printDebugI()`,
  the values for `fgText` and `bgText` are reversed.

- fgTime:

  `character` R color to colorize the time

- timeStamp:

  `logical` whether to include a time stamp in output

- formatNumbers:

  `logical` whether to format numbers using
  [`format()`](https://rdrr.io/r/base/format.html) which controls the
  number of digits displayed, and is default. When `formatNumbers=FALSE`
  sometimes `numeric` values that contain `integers` may be represented
  as `14.0000000001`.

- trim, digits, nsmall, justify, big.mark, small.mark, zero.print,
  width:

  arguments passed to [`format()`](https://rdrr.io/r/base/format.html).

- doColor:

  `logical` or 'NULL' indicating whether to colorize output. When
  `doColor` is 'NULL', if the `"crayon"` package is available, and if
  crayon detects color is permitted, color is enabled.

- splitComments:

  `logical` whether to color each element independently without
  light-dark alternating pattern. The intensity of the adjustment is
  controlled by `dex` passed to
  [`color2gradient()`](https://jmw86069.github.io/jamba/reference/color2gradient.md).

- collapse:

  `character` collapse string used to separate list items, by default ""
  so text separation is expected in the input data.

- sep:

  `character` separator used to separate vector elements, when a list
  items contains a vector.

- doReset:

  `logical` or 'NULL', indicating whether to apply
  [`crayon::reset()`](http://r-lib.github.io/crayon/reference/crayon.md)
  to the delimiter `sep`. When `doReset=TRUE` the style on the delimiter
  is forced to reset, using
  [`crayon::reset()`](http://r-lib.github.io/crayon/reference/crayon.md),
  or to remove pre-existing style with
  [`crayon::strip_style()`](http://r-lib.github.io/crayon/reference/strip_style.md).
  When `doReset=NULL` and `sep` contains ANSI escape characters, they
  are left as-is; when `doReset=NULL` and `sep` does not contain ANSI
  escape characters, `sep` becomes `crayon::reset(sep)` which forces the
  style to be reset between printed values.

- detectColors:

  `logical` whether to detect and potentially try to correct console
  color capabilities.

- dex:

  `numeric` passed to
  [`color2gradient()`](https://jmw86069.github.io/jamba/reference/color2gradient.md)
  to split a color into a lighter,darker alternating pattern. Until
  version 0.0.83.900, this process used `gradientWtFactor=1` and was not
  adjustable. Note that when `splitComments=TRUE` the input values in
  `...` are flattened to a single vector, and colors in `fgText` are
  applied directly without adjustment.

- darkFactor, sFactor:

  `numeric` arguments deprecated.

- lightMode:

  `logical` or NULL, indicating whether the text background color is
  light, where `lightMode=TRUE` indicates the background is white or
  light enough to require darker text, imposing a maximum brightness for
  colors displayed. When 'NULL' it calls
  [`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md),
  which uses:

  - `getOption("jam.lightMode")` if defined

  - otherwise attempts to detect whether the session is running inside
    RStudio, by checking for environmental variable `"RSTUDIO"`, under
    the assumption that default RStudio uses a light background,
    therefore `lightMode=TRUE`.

  - if steps above fail, it uses `lightMode=FALSE`.

  - to force a specific lightMode for all uses, use options:
    `options(jam.lightMode=TRUE)` or `options(jam.lightMode=FALSE)`.

- Crange, Lrange:

  `numeric` range of chroma and luminance values between 0 and 100. When
  NULL, default values are assigned by
  [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md).
  The intent is to restrict the range relative to the console background
  color, also controlled by `lightMode`.

- removeNA:

  `logical` whether to remove NA values and not print to the console.

- replaceNULL:

  `character` or NULL, optionally replace NULL elements with non-NULL
  character value, otherwise NULL elements are ignored.

- adjustRgb:

  `numeric` value adjustment used during the conversion of RGB colors to
  ANSI colors, which is inherently lossy. If not defined, it uses the
  default returned by
  [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md)
  which itself uses `getOption("jam.adjustRgb")` with default=0. In
  order to boost color contrast, an alternate value of -0.1 is
  suggested.

- byLine:

  `logical` whether to delimit lists by line instead of using collapse
  to combine them onto one line.

- verbose:

  `logical` whether to print verbose output

- indent:

  `character` optional characters used as a prefix to indent output.
  When `numeric` it is rounded to integer, then this many character
  spaces `" "` are concatenated together to define the indent width.
  Note that the `indent` text is not colorized.

- keepNA:

  `logical`, default TRUE, whether to keep and print NA values.

- file:

  argument passed to [`cat()`](https://rdrr.io/r/base/cat.html) to send
  output to a file or compatible output of
  [`cat()`](https://rdrr.io/r/base/cat.html). When not provided, all
  output is sent using
  [`message()`](https://rdrr.io/r/base/message.html).

- append:

  `logical` whether to append output, passed to
  [`cat()`](https://rdrr.io/r/base/cat.html) when `file` is defined.

- invert:

  `logical` indicating whether foreground and background colors should
  be switched, as is default for `printDebugI()`. Note when the
  resulting `fgText` is 'NULL', its color is defined by
  [`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md)
  to define a contrasting text color relative to the background color in
  `bgText`.

- htmlOut:

  `logical` indicating whether to print HTML span output, using format
  `<span style="color:fg;background-color:bg">text</span>`. Default NULL
  will set TRUE when knitr is running, and knitr output is HTML. When
  using inside Rmarkdown or Quarto, add chunk option: `results='asis'`.

- comment:

  `logical` whether to prefix output with ' \## ' as a comment, or
  `character` string used as a prefix. New in 1.0.5 is a leading space
  to prevent being converted to a markdown header when rendered in
  Rmarkdown or Quarto documents. The comment is useful when printed
  alongside R code, so that copy-and-paste will not include this text as
  R code. Default NULL will set FALSE when knitr is running.

## Value

NULL invisibly, this function is called for the side effect of printing
output using [`message()`](https://rdrr.io/r/base/message.html) for
console output, or [`cat()`](https://rdrr.io/r/base/cat.html) when
saving to a file.

NULL invisibly, this function is called for the side effect of printing
output using [`cat()`](https://rdrr.io/r/base/cat.html).

NULL invisibly, this function is called for the side effect of printing
output using [`cat()`](https://rdrr.io/r/base/cat.html).

## Details

This function prints colorized output to the R console, with some rules
for colorizing the output to help visually distinguish items.

The main intent is to use this function to print pretty debug messages,
because color helps identify patterns.

For use inside 'Rmarkdown' `.Rmd` and 'Quarto' 'qmd' documents, the
default condition will set 'htmlOutput=TRUE' when knitr is producing an
HTML output file, and 'htmlOutput=FALSE' otherwise. The argument
'comment' now uses a leading space ' \## ' to prevent being interpreted
as a markdown heading, however when run inside knitr, the default is
'comment=FALSE'.

In 'Rmarkdown' and 'Quarto', define the chunk option `results='asis'` to
enable properly colored text.

By default, output has the following configurable properties:

- Each line begins with a comment, controlled by default
  `comment=getOption("jam.comment", TRUE)` with default ' \## '. It can
  be customized, or `FALSE` for no prefix at all.

- Each line includes time and date stamp controlled by
  `timeStamp=getOption("jam.timeStamp", TRUE)`, by default the current
  time and date.

- Each line formats `numeric` values, controlled by
  `formatNumbers=getOption("jam.formatNumbers", TRUE)`, which determines
  whether to apply arguments `big.mark` and `small.mark` to make numeric
  values more readable.

- Each entry in `...` is printed with its own foreground color `fgText`,
  background color `bgText`, with a slight lighter/darker dithering
  effect to add minor visual distinction for multiple values.

- Values in each `vector` are concatenated by `sep=","` by default.

- Each `list` is concatenated by `collapse=""` by default.

Additional convenience rules:

- For convenience, when the last `...` argument is a `character` vector
  of colors, it is assumed to be `fgText`.

- When the only entry in `...` is a `character` vector of R colors, the
  names are printed using the color vector for `fgText`, or if no names
  exist the colors are printed using the color vector for `fgText`.

- For `printDebugI()` or `invert=TRUE`, colors typically assigned to
  `fgText` are instead assigned to `bgText`.

- For very specific color assignments, `fgText` and/or `bgText` can be
  defined as a `list` of `character` vectors of R colors, in which case
  the `list` overall is recycled to the length `...` to be printed, and
  within each vector of `...` printed the corresponding color vector is
  recycled to the length of that vector.

The `printDebugI()` function prints colorized output to the R console,
using the same logic as `printDebug` except by default the color is
inverted so the default `fgText` colors are applied to the background.

The `printDebugHtml()` function prints colorized output in HTML form,
using the same logic as `printDebug()` except by default the output is
HTML. It also sets 'comment=FALSE' by default.

Set the Rmarkdown or Quarto chunk option `results='asis'` which causes
the HTML code to be interpreted directly as HTML.

The `printDebugHtml()` function internally calls `printDebug()` which
then calls
[`make_html_styles()`](https://jmw86069.github.io/jamba/reference/make_html_styles.md).
The text is surrounded by `<span color='#FFFFFF'>` HTML formatting.

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
[`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md),
[`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md),
[`lldf()`](https://jmw86069.github.io/jamba/reference/lldf.md),
[`log2signed()`](https://jmw86069.github.io/jamba/reference/log2signed.md),
[`middle()`](https://jmw86069.github.io/jamba/reference/middle.md),
[`minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md),
[`newestFile()`](https://jmw86069.github.io/jamba/reference/newestFile.md),
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
printDebug("Testing ", "default ", "printDebug().");
#> (16:01:27) 25Jul2026: Testing default printDebug().
printDebug("List of vectors:", c("one", "two", "three"));
#> (16:01:27) 25Jul2026: List of vectors:one,two,three

# By default, there is no space between separate elements in `...`
printDebug("List of vectors:", c("one", "two", "three"),
   c("four", "five", "six"));
#> (16:01:27) 25Jul2026: List of vectors:one,two,threefour,five,six
# To add a space " " between elements, use collapse
printDebug("List of vectors:", c("one", "two", "three"),
   c("four", "five", "six"), collapse=" ");
#> (16:01:27) 25Jul2026: List of vectors: one, two, three four, five, six

# slightly different style, one entry per line, indented:
printDebug("List of vectors:", c("one", "two", "three"),
   c("four", "five", "six"), collapse="\n   ");
#> (16:01:27) 25Jul2026: List of vectors:
#>    one,
#>    two,
#>    three
#>    four,
#>    five,
#>    six

# when a vector entirely contains recognized colors,
# the colors are used in the output
printDebug(c("red", "blue", "yellow"));
#> (16:01:27) 25Jul2026:    red,blue,yellow

# When the vector contains colors, the names are used as the label
color_vector <- jamba::nameVector(c("red", "blue", "green","orange"),
   c("group_A", "group_B", "group_C", "group_D"));
printDebug(color_vector);
#> (16:01:27) 25Jul2026:    group_A,group_B,group_C,group_D

# Remember the sister function that inverses the colors
printDebugI(color_vector);
#> (16:01:27) 25Jul2026:    group_A,group_B,group_C,group_D

printDebug(1:10, fgText="blue", dex=2);
#> (16:01:27) 25Jul2026: 1,2,3,4,5,6,7,8,9,10
printDebug(1:10, bgText="blue", dex=2);
#> (16:01:27) 25Jul2026: 1,2,3,4,5,6,7,8,9,10
printDebug(1:10, fgText="orange", dex=2);
#> (16:01:27) 25Jul2026: 1,2,3,4,5,6,7,8,9,10
```
