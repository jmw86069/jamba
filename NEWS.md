# jamba version 0.0.29.900

## new functions

* `writeOpenxlsx()` is a wrapper for the `openxlsx` package,
intended to encapsulate a large number of default options for
certain "known" column data types, like P-values, log2 fold changes,
and integer values.
* `applyXlsxCategoricalFormat()` and `applyXlsxConditionalFormat()` to
apply colors to an existing Excel xlsx file worksheet. These functions
are called by `writeOpenxlsx()` but can also be called manually to
customize an existing Excel file.

## changes

* Added "openxlsx" to Suggests.

# jamba version 0.0.28.900

## bug fixes

* `asSize()` was updated to handle empty units, useful for numeric
values especially when kiloSize=1000. Numbers are also trimmed
through `format()` independently so the number of digits is
not propagated through all values.

# jamba version 0.0.27.900

## enhancements

* `setTextContrastColor()` has been updated to handle alpha
transparency, semi-transparent colors are blended with the
background color, then compared to the threshold.

# jamba version 0.0.26.900

## new functions

* `formatInt()` is a quick wrapper function to format numeric values
as integers, by default rounding decimal values and using big.mark=",",
which can be overridden as needed.
* `list2df()` to convert a list of vectors into a tall data.frame.

# jamba version 0.0.25.900

## bug fixes

* `warpAroundZero() was not properly handling `baseline`, the method
now uses a more straightforward approach.

## enhancements

* The site pkgdown documentation was updated to include each function
in one (or sometimes multiple) categories, to help in finding
potentially useful functions.
* The README.Rmd file has been stripped of the examples, which were
a bit out of date compared to the function docs, and were not intended
to be their own vignette. As I write this, I realize "removed visual examples"
is rarely a positive step.

# jamba version 0.0.24.900

## new functions

* `pasteByRowOrdered()` is an extension to `pasteByRow()` which creates
an ordered factor, where the individual column order is maintained, either
using existing factor level ordering, or via `mixedSort()` per column.

# jamba version 0.0.23.900

## new functions

* `unalpha()` simply removes the alpha transparency from an
R hex color, which is required for some plotly functions
for example.
* `isTRUEV()` and `isFALSEV()` are vectorized forms of `isTRUE()`
and `isFALSE()`. As they take vector input, the entire vector must
be `logical` in order for any entry to be considered.

## changes to existing functions

* `rgb2col()` was enhanced so the `alpha` argument conveys whether
to return alpha transparency in the output string. It seemed to
be a convenient place to control alpha since the conversion of
color names to hex requires converting to RGB as an intermediate,
then back to hex color.

# jamba version 0.0.22.900

## changes to existing functions

* `minorLogTicks()` and `minorLogTicksAxis()` had an argument renamed
to `offset` instead of `labelValueOffset`. Better to change it now before
it has wider use. Also, the defaults were changed to `logBase=2` and
`displayBase=10` based upon the current most frequent usage. Best to
set these values explicitly. Ideally, R graphical parameters
should define the log base instead of TRUE/FALSE in `par("xlog")`
and `par("ylog")`.
* `normScale()` was modified so the `low` and `high` range values
are properly honored even when supplying a single value for `x`.
Now the behavior checks if `low` is equal `high` to determine
whether to use the `low` and `high` values. This adjustment helps
`applyCLranges()` restrict colors to allowed ranges without
over-correcting single colors to the midpoint of the range.

## new functions

* Added function `sqrtAxis()` which computes axis tick mark
positions for data that has been square root transformed, using
transformation `sqrt(abs(x))*sign(x)` in order to maintain positive
and negative values. This function is called by
`plotPolygonDensity()` when using `xScale="sqrt"`.

## bug fixed

* Cleaned up an issue with `getColorRamp()` with RColorBrewer colors,
where it previously would retrieve colors using a number higher then
the actual number supplied by `RColorBrewer::brewer.pal()`, and
which only issues a warning (which I had hidden.) The code now checks
for the proper maximum number, and expands to the larger number using
`colorRampPalette()` as needed.

# jamba version 0.0.21.900

## bug fixes

* Updated `cPaste()` to handle lists with a single NA value, which
failed previously because a single NA is considered class "logical"
and not "character" as required by `S4Vectors::unstrsplit()`.
* Changed how `Crange` and `Lrange` values are used in `make_styles()`,
the new default will not overwrite the global `Crange` and `Lrange`
settings.
* Changed how `Crange` and `Lrange` values are enforced by
`applyCLranges()`. There is a new argument `CLmethod` which controls
how C and L values are adjusted.

# jamba version 0.0.20.900

## new functions

* `fillBlanks()` is used to fill a vector that has missing values,
and fills missing values with the most recent non-blank value. It is
useful when importing Excel data where headings might be present in
the first cell of a block of cells, followed by blanks.
* `newestFile()` takes a vector of files, and returns the most
recently modified, using `file.info()`.

# jamba version 0.0.19.900

## new functions

* `grepls()` a useful utility for search the active environment by
object name. For example `grepls("statshits")` will find everything
named "statshits", or `grepls("farris")` will find everything with 
"farris" in the name", even within packages, or other attached
environments in the search space.
* `warpRamp()` takes a vector of colors as from a color gradient,
and warps the gradient. For divergent colors, the adjustmet is
symmetric around the middle color; otherwise the adjustment is relative
to the first color. Helpful for adjusting colors scales in heatmaps.

# jamba version 0.0.18.900

## bug fixes

* `imageDefault()`, `plotSmoothScatter()`, `smoothScatterJam()` were
updated to allow values lower than 200 for `nbin` which previously caused
problems when `fixRasterRatio=TRUE` and `oldstyle=TRUE`.
* `rmNA()` by default returns `NULL` when given `NULL` input, unless
`nullValue` is defined. This change fixed several warnings, and
resolved inconsistencies with `setCLranges()` not handling NULL
parameters properly, consequently affecting `make_styles()` ability
to restrict the color chroma (C) and luminance (L) ranges, seen
when `printDebug()` did not use scaled colors.
* Fixed bug in `hcl2col()` during detection of RGB values above 255,
which sometimes happens during the `colorspace::polarLUV()`
conversion from HCL to RGB.

## new functions

* `rlengths()` returns the recursive lengths of list elements, intended
when a list may have multiple nested lists. It returns the
top level of counts per list element by default, but with `doSum=FALSE`
it returns the full structure with the length of each non-list element.


# jamba version 0.0.17.900

## new functions

* `minorLogTicksAxis()` and `minorLogTicks()` provide log-transformed
axis labels, with the added benefit of handling `log(1+x)`
transformations properly; or log2FoldChange data with symmetry around
zero.
* `renameColumn()` a basic function but which allows re-running
the function without error.
* `normScale()` scales a numeric vector into a fixed range, by default
between 0 and 1. If one value is given, a parameter `singletMethod` defines
whether to use the minimum, maximum, or mean of the range.
* `rowGroupMeans()` and `rowRmMadOutliers()` are used to compute
per-row mean values, with grouped columns. Optional outlier detection
is performed using a MAD factor cutoff, for example 5xMAD threshold means
points 5 times the MAD for the group are considered outliers.
* `plotPolygonDensity()` which is a wrapper around `hist()` and
`plot(density(x))`, but with some added features like scaling the x-axis
by log10 or sqrt; and multi-panel output when the input is a multi-column
matrix.
* `warpAroundZero()` takes a numeric vector, and warps the values with
a log curvature, symmetric around zero. The intent is to create non-linear
breaks when used in heatmaps with divergent color ramps.

## bug fixed

* `ssdim()` was updated again to try to handle more non-list object
types, for example the "bga" class returned from made4::bga().


# jamba version 0.0.16.900

## bug fixes

* Fixed small issue with R docs that did not properly wrap the `%>%` inside
`\code{\%>\%}` blocks, which allows the percent sign to be escaped and
ignored.
* Fixed numerous incorrectly formatted `\code{\link{}}` sections which did
not properly specify the package name. The issue apparently only causes
errors during package install on systems using HTML as the preferred help
format.


# jamba version 0.0.15.900

## bug fixes

* `smoothScatterJam()` and `imageDefault()` were updated to fix edge
cases where the image being rendered was expected to have integer
axis units, but where the raster ratio had duplicated columns or
rows, making the units imprecise.
* `setPrompt()` has default `projectName=NULL` instead of trying to
pull from `projectName` from the `.GlobalEnv` environment, which
would cause an error if not defined. Instead, if `NULL` then it
checks if `projectName` exists, if so the value is used.

# jamba version 0.0.14.900

## bug fixes

* Fixed issue with `sdim()` so it would properly traverse a non-vector
and non-list object which may contain elements with length or dimension.

# jamba version 0.0.13.900

## enhancements

* `setCLranges()` was adjusted to use global options where available,
and by default will update global options when not defined. This mechanism
allows one to set specific Crange and Lrange values, and have them be
re-used by functions affected by those values.
* `ssdim()` now handles S4 objects by iterating each slotName and calling
`sdim()`.
* `sdim()` now properly handles detection of S4 classes.

## new functions

* `fixYellowHue()` takes a matrix of HCL colors (as from `col2hcl()`) and
adjusts colors between hue 80 and 90 so they appear less green. This change
is a visual preference that the default yellow appears green when darkened,
and simply makes the color appear less green.
* `fixYellowColor()` takes a vector of colors as a wrapper to `fixYellowHue()`.

# jamba version 0.0.12.900

The main change in this version was to reduce required R version to 3.0.0.

# jamba version 0.0.11.900

## new functions

* `cPaste()` is an efficient method of pasting a list of vectors
using a delimiter, to create a character vector. It uses `mixedOrder()`
to sort entries, and optionally applies uniqueness to the vectors.
* `uniques()` takes a list and makes each vector in the list unique,
using an efficient mechanism that applies uniqueness to the overall
set of values at once.

## Suggests S4Vectors

* The Bioconductor package `S4Vectors` implements efficient `unstrsplit()`
used by `cPaste()`. However, S4Vectors therefore requires installing
Bioconductor base packages, which might be a heavy installation requirement
just for improved efficiency at this step. In future this one function
may be reproduced here (with permission of author Dr. Pages) to
reduce the dependency burden.

# jamba version 0.0.9.900

## enhancements

* `jargs()` does a better job of handling functions with nested lists,
   and closes any open `[` with corresponding `]`.
* `jargs()` properly honors `useColor=FALSE` to disable colorized text,
   and adds `colNULL` to assign a color for `NULL` values, by default grey.
* `printDebug()`, `make_styles()` will apply a CL range, chroma (C) and
   luminance (L) values, which are defined in part by `lightMode` and
   corresponding function `checkLightMode()`, and a helper function
   `setCLranges()` which sets sensible default values for light and
   dark backgrounds.
* `printDebug()` has two optional parameters `Crange` and `Lrange` which
   define a chroma (C) range and luminance (L) range. For lighter background,
   a maximum luminance of 80 seems reasonable; for darker background colors,
   minimum luminance of 75 is effective. The only awkwardness is that yellow
   darkens to a greenish hue, which is not aesthetically pleasing, so
   because I even noticed it, I had to "fix" the issue by adjusting yellows
   to more gold before darkening, enabled with default argument
   `fixYellow=TRUE`.
* `make_styles()` calls `applyCLrange()` which fixes the chroma (C) and
   luminance (L) values, hopefully to sensible default values.

## New functions
* `applyCLrange()` takes a vector of colors, and fixes the chroma (C) and
   luminance (L) values to this range. If `Cgrey` is above zero, then colors
   with chroma (C) value below this threshold are kept unchanged, in order
   to prevent a purely grey (unsaturated) color from becoming colorized.
* `setCLranges()` will query `checkLightMode()` and return `Crange` and
   `Lrange` default values.
* `hcl2col()` which simply converts the output of `col2hcl()` back to
   a vector of R hex colors. It should not be lossy, meaning it should
   faithfully convert R colors to an HCL matrix, then back to the
   same R original hex colors. Alpha transparency is also maintained.
   Note that `hcl2col()` performs differently than `grDevices::hcl()`
   which does not produce the same colors as `colorspace::polarLUV(H, C, L)`
   using the same values for `H`, `C`, and `L`. The reasons unclear,
   however the functions in `jamba` are internally consistent, using
   `colorspace` functions.

# jamba version 0.0.6.900

## enhancements

* jamba now uses some global options(), intended only for function parameters
   that affect visual output, and will not affect analysis processing.
   Examples:
   * `jam.adjustRgb` - numeric, sets the parameter "`adjustRgb`" in printDebug().
   * `jam.lightMode` - boolean, sets the lightMode, see below.
* `printDebug()` has a new boolean parameter "lightMode", which is TRUE when the
   background color is light (e.g. white or light-grey.) In this case, it
   applies a ceiling to the brightness of all text colors to ensure none are
   too bright to be visible.
* `mixedSortDF()` now accepts character values for `byCols`, with optional prefix
   "-" to indicate decreasing sort. Alternatively, the parameter decreasing
   will still be used to reverse the sort, and is converted to a vector when
   byCols has multiple values. Note that the prefix "-" and decreasing are
   multiplied to combine them.
* `smoothScatterJam()` tests if postPlotHook is a function, in which case it
   runs `postPlotHook(...)` otherwise it simply evaluates `postPlotHook;`
* `nullPlot()`, `usrBox()`, and `imageDefault()` have a new boolen parameter
   "`add`" indicating whether to create a new plot or add to an existing
   plot device.
* `rbindList()` has a new boolean parameter `keepNA` indicating whether to keep
   NA values in the results, which ultimately causes NA to be converted to
   "NA".
* `makeNames()` has a new boolean parameter `keepNA` indicating whether to keep
   NA values in the results. When keepNA is TRUE, NA is converted to "NA",
   otherwise NA entries are treated as "" prior to creating names.
* `rmNA()` changed the default value for parameter `rmNAnames` to `FALSE`, which
   was the more anticipated behavior. An input vector will not be shortened
   for non-NA values that have an NA name. Changing function parameter
   defaults will be an extremely rare occurrence in future.
* `make_styles()` which is a vectorized wrapper for `crayon::make_style()`,
   gains some finer control over color chroma and luminance.
* `jargs()` gets a larger refactoring, aimed at better display of list
   parameters, and lists of lists and other nested variations. It now does
   a better job of displaying the list and vector names appropriately.
   A new function `handleArgsText()` was split into a separate function, but
   is a rare non-exported function since it is currently only useful for
   `jargs()`.
* TODO.md was created to keep track of future enhancements and fixes.

## new functions

* `applyLceiling()` intended to restrict colors to a maximum luminance in HCL
   color space, mainly to support the new lightMode option intended for using
   `printDebug()` with a light background.
* `checkLightMode()` intends to support the new lightMode option by doing its
   best to check situations to enable lightMode. It first checks
   options("lightMode"). It then checks environment variables that suggest
   Rstudio is running, in which case it defaults to lightMode=TRUE, since
   the default Rstudio has a light background.
* `sclass()`, `sdim()`, and `ssdim()` are intended to help handle list objects.
   * `sclass()` returns the class of each list element.
   * `sdim()` returns the dimensions (or lengths) or each list element.
   * `ssdim()` is a special case that returns the dimensions of a list of
   list of objects.
   In all cases, if the input object is an S4 object, it operates on
   `slotNames(x)`. Thus, calling `ssdim(x)` is helpful for S4 objects, since it
   returns the class and dimensions of each object inside the S4 object.

