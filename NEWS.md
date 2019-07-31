# jamba version 0.0.39.900

## changes

* `drawLabels()` first argument is `txt` which is the label
to be displayed, a small convenience change.
* `breaksByVectors()` examples were updated to fix a typo
when calling `adjustAxisLabelMargins()`.
* Added substantial content to the vignette, including moving
multiple images to `docs/articles` for pkgdown.

## possible breaking changes

* `cPaste()` default `doSort=TRUE` is changed to `doSort=FALSE`.
New functions: `cPasteS()` is intended for sorted values;
`cPasteU()` is an alias for `cPasteUnique()` and is intended
for unique values; `cPasteSU()` is intended for sorted unique
values. They all call `cPaste()` but with convenience defaults.
It is recommended to use `cPasteS()` to replace sorted `cPaste()`
in cases where output is expected to be sorted. The change was
made now while impact is limited mostly to other Jam packages
and could be mitigated.

## new function

* `groupedAxis()` draws grouped axis labels, a small convenience
function that extends `breaksByVectors()`.


# jamba version 0.0.38.900

## bug fixes

* `provigrep()` was enhanced to handle duplicated input entries,
which previously returned only the unique entries, but now correctly
returns all entries including duplicates, in the proper order.
The optional list output also makes each list element unique.
New argument `value` allows returning index positions, which
is equivalent to `proigrep()`. Lastly, the `ignore.case` argument
is now properly honored, in order to allow case-sensitive matching.
* `checkLightMode()` will not try to use `rstudio::getThemeInfo()`
if that package is installed, and if the function API exists,
in order to determine whether Rstudio is currently using a dark
theme.
* `make_styles()` has a new optional argument `bg_style` which
allows defining the foreground `style` and background `bg_style`
in one step. When `bg_style` is supplied, the Crange and Lrange
arguments are ignored. Fixed a bug when rendering in Rstudio,
where an ANSI foreground color of white does not get properly
reset, causing all subsequent foreground colors to be white despite
clear ANSI codes for different colors. The workaround is to render
white as slightly off-white (greyscale 254 instead of 255) which
restores correct output.
* `printDebug()` was enhanced to use the `bg_style` argument
of `make_styles()`. New argument `invert` which will switch
foreground and background colors. Fixed some follow-up issues
with handling empty strings.
* `cPaste()` fixed issue with `cPaste(NULL)` and
`cPaste(list(NULL))` which previously caused an error, now
returns `""` consistent with output from `cPaste(list(NULL, letters[1:2]))`
where NULL entries produce `""`.
* `fixYellow()` new argument `fixup` passed to `hcl2col()` which
fixes out of gamut colors to scale within viewable range.
* `writeOpenxlsx()` was not properly saving rownames with argument
`keepRownames=TRUE`, this issue was resolved.

# jamba version 0.0.37.900

## bug fixes

* Fixed a typo in `make_styles()` where the default
Lrange was `getOption("jam.Crange")` instead of
`getOption("jam.Lrange")`. The affect is limited to
debugging output when printed on a dark background.
* `asSize()` was updated to handle `object_size` above
2.1e9 (2 Gigabytes), it previously converted to integer
which does not handle values that large, but now converts to
numeric.

## new functions

* `log2signed()` performs log2 transformation of the
magnitude of the numeric values, while preserving the
directionality. It effectively performs `log1p(abs(x))*sign(x)`
except that by default it uses log base 2.
* `exp2signed()` is the reciprocal to `log2signed()`, it
exponentiates the magnitude of the numeric values, while
preserving the directionality.

## changes

* Cleaned up unnecessary verbose output from `minorLogTicks()`.

# jamba version 0.0.36.900

## bug fix

* Fixed embarrassing type in R package dependencies,
"S4Vector" instead of "S4Vectors".

# jamba version 0.0.35.900

## changes to existing functions

* `setTextContrastColor()` default hclCutoff was changed
from 73 to 60, in order to use dark text more often for
the "in between" colors.
* `makeNames()` now uses `duplicated()` to detect duplicates
prior to assigning names, which is substantially faster for
long vectors where not all entries are duplicated, when
compared to applying `table(x)` to the entire vector.
For large vectors with mostly duplicated values, the
speed is the same as before. Note this change also affects
`nameVector()`.
* `sdim()` now handles a special case of S4 object where data
is stored in one slot with slotName `".Data"`, for example
`"limma::MArrayLM-class"` uses this format. When the length
of `names(x)` matches the length of `slot(x, ".Data")` then
those names are applied.
* `sclass()` now properly detects S4 objects, and specifically
recognizes `data.frame` as a list even though it secretly has
`slotNames` like an S4 object, while not being an S4 object.
(Is it?) Matrix-like objects return the class of each column
for consistency, even though all columns have identical class.
This function also handles the same special case S4 scenario
when it has only one slotName `".Data"`, as described above
for `sdim()`.

## bug fixes and changes to mixedOrder() family of functions

* `mixedOrder()` did not sort properly when any entry contained
only `"-"` dashes, this problem has been fixed. Previously "-"
was potentially used to indicate negative numbers and therefore not
treated as a blank value, inadvertently becoming `NA` and disrupting
the ordering. Now strings `"-"` and `"---"` are considered blanks.
* `mixedOrder()` now uses the input string as a final tiebreaker,
so blank strings like `"--"` and `"-"` will also be sorted
`c("-", "--")` in the output instead of returning in the same
order as the input.
* `mixedOrder()`, several regular expressions were updated to cover
edge cases, and will soon be wrapped into a series of `"testthat"`
unit tests, long overdue. Also, verbose output was slightly updated.
Specifically:

    * blanks are properly ordered at the front when
    `blanksFirst=TRUE`, and at the end when `blanksFirst=FALSE`.
    * NA values are also properly ordered at the front and end
    with `NAlast=FALSE` and `NAlast=TRUE` respectively. The NA order
    has priority over blanks, so NA will always be completely first
    or completely last.
    * Infinite values, when `keepInfinite=TRUE` will be ordered at the
    end, but before blanks, then before NA values, if those values
    are also positioned at the end. Also, when `keepNegative=TRUE`
    then `"-Inf"` will be position at the beginning, but after NA
    then after blanks, if those values are also positioned at the
    beginning.
    * `keepNegative=TRUE` also enables recognition of scientific
    notation, but that regular expression wrongly allowed decimal
    exponentials, which is not valid (e.g. "1.23e1.2" is not valid.)
    now the exponential only includes non-decimal numeric values,
    e.g. `"-1.23e2.2"` is effectively considered `"-1.23e2"` and `"2"`.
   
* `mixedSort()`, `mixedSorts()` and `mixedSortDF()` are all affected
by the changes to `mixedOrder()`.
* `mixedSorts()` was updated to enable correct behavior when
`sortByName=TRUE`, which only works when all vectors in the input list
are named.
* `mixedSorts()` will accept nested lists, and would have used
the ultra-cool `utils::relist()` function, except that function does
not allow re-ordering the vector names within the nested list. So
a new function `relist_named()` was added to jamba.
It also works with `sortByName=TRUE` to sort each
vector by its names, by only if all vectors are named.

## new functions

* `relist_named()` is a small modification of the `utils::relist()`
generic function (actually mostly just the `utils:::relist.default()`).
This function splits a vector into a list matching the structure of
a skeleton list, except that the names of each vector in the
output list will match the input vector `x` and not the names from
the vectors in the skeleton list.


# jamba version 0.0.34.900

## changes to existing functions

* `cPaste()` now properly handles a list with factor and non-factor
types, previously `base::unlist()` coerced the mixed list to character
which changed factor values to integers when non-factor elements were
present. Now `keepFactors=TRUE` will preserve factor levels when
sorting with `doSort=TRUE`.

# jamba version 0.0.33.900

## new functions

* `gsubOrdered()` applies `base::gsub()` to character or factor input,
and returns a factor output, retaining the order of levels based upon
the input.

# jamba version 0.0.32.900

## changes to existing functions

* `jargs()` changed the default `sortVars=FALSE` which prints function
arguments in the order they are defined.

## new functions

* `mergeAllXY()` merges multiple data.frames, keeping all rows and
columns.
* `unnestList()` flattens a nested list-of-lists into a simple, flat list.

# jamba version 0.0.31.900

## bug fixes

* `makeColorDarker()` fixed issue with repeated colors and darkFactors,
processed using unique combination of color,darkFactor,sFactor to improve
efficiency with large vectors, but was not properly handling the unique
vectors.

# jamba version 0.0.30.900

## changes

* revised help text for `uniques()`
* `col2hcl()` uses `colorspace::sRGB()` instead of `colorspace::RGB()` to
match the convention that `colorspace::hex2RGB()` actually uses *sRGB*
and not *RGB* as the function name (and all common sense in the realm
of how to name a function) implies.
* `col2hcl()` and `hcl2col()` allow selecting from `polarLUV` or
`polarLAB` color models, both of which use HCL values to generate
colors.
* `col2hcl()` no longer forces the output vector to have names, however
it retains names if provided in the input.
* `hcl2col()` will use `polarLUV` or `polarLAB` as input without
converting to an intermediate color space, which makes output slightly
less lossy than converting to and from sRGB.

## new functions

* `ucfirst()` is a simple function to uppercase the first letter in
a word or phrase.
* `mixedSorts()` will sort a list of vectors using `mixedSort()` alphanumeric
sort, and fairly efficiently too! It sorts the unlisted data then splits
it back to a list.
* `drawLabels()` and `coordPresets()` were moved from the `jamma` package.
The `drawLabels()` can be used to add text labels to a plot, with
convenient coordinate presets handled by `coordPresets()` which include
things like "topleft", "center", or "right". It automatically adjusts the
text position to stay inside the plot border in those cases.


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

