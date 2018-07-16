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

