## TODO for jamba


### implement testthis unit testing


### new functions

* `splomSmooth()` is a `plotSmoothScatter()` equivalent for
`lattice::splom()`, with some default settings and usability
to help make it easier to call than `lattice::splom()`. It
is also much more efficient than ggplot2 equivalent methods,
despite substantial efforts. Go with what works! (For now.)


### Bugs 28aug2020

* FIXED: `printDebug()` throws a warning about fixYellow:

> Warning message:
> In if (fixYellow) { :
>   the condition has length > 1 and only the first element will be used


### Bugs 29jul2020

* `writeOpenxlsx()` highlightColumns appears to prevent conditional
formatting on the same columns.
* `provigrep()` checks for names and assigns only if missing, it does
not correct duplicate names if they are already present.

### enhancements 29jul2020

* `tcount2()` - wrapper for `tcount(..., minCount=2)`

### migrate functions from other packages

* `find_colname()`
* `deconcat_df()`


### Enhance writeOpenxlsx() 08may2020

* `writeOpenxlsx()` option to supply a header which is displayed in
the top row of the Excel worksheet, with its own style. For example
the header may be a title "Data following normalization and limma analysis"
with dark blue background, white text, font size 18.

#### Nice to have

* `applyXlsxConditionalFormat()` should in theory color the text
with `setTextContrastColor()` to use white text on dark background
colors. Not sure if Excel allows it, or if it requires manually
setting the color in each cell.
* `writeOpenxlsx()` option to apply conditional formatting
to numeric columns using each column numeric range, not fixed range.
Other option is to supply a list of rules for `numRule`. Design idea:
When `numRule=NULL` or `numRule=NA` then create rule based upon
the numeric range (min, mean, max). Also, `numRule` can be a `list`,
for example `numRule=list(c(0,100,1000), NA, c(0,1,2))`. This rule
would apply `c(0,100,1000)` to the first column in `numColumns`,
then would apply `NA` to the second column (thus auto-scaled),
then would apply `c(0,1,2)` to the third column.

### Optimizations 29apr2020

* FIXED: `cPaste()` is not fully efficient when the input does not
require going through steps `unlist()` then `split()`. In these
cases the data should be left as `CharacterList` if possible,
to use `S4Vectors::unstrsplit()` directly. Conditions that
require `unlist()` then `split()`:

   * `na.rm=TRUE` and the presence of `NA` values
   * `doSort=TRUE` and any list element with multiple values

### Bug 02oct2019

* `plotSmoothScatter()` throws an error
`"is.numeric(xlim) is not TRUE"`
when plotting data with class `"difftime"`.

### General enhancements

* Add message during package install suggesting the "crayon" package be
installed if not already installed.
* Investigate the interplay between Mac/Linux TERM,
locale with LC_ALL, special characters like <ce> (theta),
and R help docs.

### Bug fixes / Enhancements to existing functions

1. `printDebug()` probably needs a specific delimiter between
lists. Currently `collapse` is accepted via `...` but separates
every entry.

### new plotSmoothScatterG with grouped colors

1. `plotSmoothScatterG()` a new function which would create a
smooth scatter plot like `plotSmoothScatter()` except that it will
also allow coloring points by group. The previous closest estimate
was the ability to overlay contours of different colors, however
the transparency of each layer is not effective, the last color
layer drawn becomes the dominant color.
This function essentially creates a layer for each color, then
blends them into one collective layer. Each layer should probably
be a `weight` applied to each layer of colors, in order to
avoid blending the paleness become dominant. The point color will
be determined by the weighted color blend, the intensity of the color
as a gradient starting from background color will be determined by
the sum of the weights, relative to the rest of the plot.

Required secondary functions:

* Weighted color blending function. Must blend yellow and blue to
make green, not grey (my own requirement). See `"jonclayden/shades"`,
or `colorspace::mixcolor()` though it only works with 2 colors.
* Weighted gradient color assignment, such that given any color,
and a weight from 0 to 1, returns a single color representing the
color at that position along the gradient from background to
foreground color.

Commentary: Does any one color space blend `red + yellow = orange`
and `blue + yellow = green`? Maybe `polarLUV`. Others either blend
`blue + yellow` into `grey` or `purple` ... makes zero sense (to me).

2. `printDebug()` option for HTML or Rmarkdown-friendly output.
Note this feature seems to work when producing HTML output from Rmarkdown.

3. `jargs()` is choking on certain argument formats:

    * When an argument is a prefixed function, the `::` should not have
    spaces before and after. `f <- function(x=jamba::colorjam){10};jargs(f);`
    It doesn't happen when the function has parentheses:
    `f <- function(x=jamba::colorjam(a)){10};jargs(f);`
    * There was another case of an empty argument being displayed where
    an actual argument existed for the function. I can't remember the
    exact conditions, but this is a placeholder for now.

### testthat

Functions in `jamba` should have their various options tested
using the package `testthat` to help maintain consistent performance
for future package and R builds.

* `jargs()` can be tested with several example custom functions to make
sure it produces output as desired.
* `mixedSort()`, `mixedSortDF()`, `mixedOrder()`, `mixedSorts()`
needs several tests to confirm consistent outputs for the various
custom conditions.



### Functions to add


### jam global re-usable options

The goal is to make it easier to set certain function parameters once as
a global option, to make it more efficient for these parameters to be used
in several functions. Per Hadley Wickham's recommendation, options should
be restricted to non-analytical parameters, in order to maintain
clear reproducible analysis workflows. That is, any analysis parameter should
be defined clearly when the function is called, and should never use the
value of a global option.

In fact, options that only change visual output may be preferred as global
options, since they help support specific output and should not be hard-coded
into the R script.

In general, visual themes may be good candidates for these parameters.

#### Nomenclature for jam package options

Jam package options should be named with the prefix "jam." followed by the
name of the function argument. For example the function parameter "adjustRgb"
would become "jam.adjustRgb".

#### Parameters proposed to become jam options

* **jam.adjustRgb**, used by colsHead() and printDebug() when displaying
colored text on the R console. This value applies a slight adjustment to the
crayon package rgb-to-ansi conversion to correct the limitation of
256-color ANSI having only 6 channels of brightness for red,green,blue,
not 8 each as it typical for VGA-style 256-color palettes. The effect of
6 channel color ranges is that the rgb conversion results in fractional
values which are rounded up, making many colors less saturated (less colorful),
which by nature works against the core goal of adding color.
* **jam.lightMode**, used by colsHead() and printDebug() to restrict the range
of dark or light colors displayed on an R console, based upon whether the
background colors is light (high luminance, lightMode=TRUE), or dark
(low luminance, lightMode=FALSE.) Normally, if lightMode is not specifically
defined, functions will try to detect whether running inside Rstudio, and
if so it assumes Rstudio is using the default white background, and sets
lightMode=TRUE.

### rbindList

* optional recursive operation, in the event of a nested list of data.frames,
it should recurse through the list, calling rbindList() on each list element
and progressive build up one resulting data.frame or matrix.
* optional ability to handle different colnames; alternative is to review
`data.table::rbindlist()` as potential replacement.

### imageByColors

* new argument `cellnoteColor` to define specific cellnote text color,
currently uses `setTextContrastColor()` but should be able to define custom
colors. (Setup for print colorized text data.frame using similar input.)
* optional boolean parameter to transpose the image, i.e. t(x) and
   if applicable t(cellnote).
* optionally draw boxes around grouped labels, a visual indicator of
   groups of cells sharing one label. Care should be taken not to enable
   this functionality with a large table containing no grouped labels, or
   even with any scenario resulting in "too many boxes".
   This option likely needs SpatialPolygons and some related
   function like `rgeos::gUnion()`. Is `rgeos` a reasonable package
   dependency, or is that too heavy for this purpose?

### color brightness and saturation handling

* helper functions like darken(), lighten(), saturate(),
   desaturate(). Note colorspace::desaturate() completely removes all color
   saturation (chroma), and conflicts with this function naming scheme.
   Maybe: `jam_desaturate()`, `jam_darken()`, `jam_brighten()`, `jam_saturate()`.
* new function `subsetColors()` which internally creates a data.frame with
   hex, RGB, HSV, and HCL values, which can then be used to subset an input
   set of colors. Bonus points for accepting different color classes at
   input, and returning the same color class at output.

