# jamba version 0.0.6.900

## enhancements

* jamba now uses some global options(), intended only for function parameters
   that affect visual output, and will not affect analysis processing.
   Examples:
   * jam.adjustRgb - numeric, sets the parameter "adjustRgb" in printDebug().
   * jam.lightMode - boolean, sets the lightMode, see below.
* printDebug() has a new boolean parameter "lightMode", which is TRUE when the
   background color is light (e.g. white or light-grey.) In this case, it
   applies a ceiling to the brightness of all text colors to ensure none are
   too bright to be visible.
* mixedSortDF() now accepts character values for byCols, with optional prefix
   "-" to indicate decreasing sort. Alternatively, the parameter decreasing
   will still be used to reverse the sort, and is converted to a vector when
   byCols has multiple values. Note that the prefix "-" and decreasing are
   multiplied to combine them.
* smoothScatetrJam() tests if postPlotHook is a function, in which case it
   runs `postPlotHook(...)` otherwise it simply evaluates `postPlotHook;`
* nullPlot(), usrBox, and imageDefault() have a new boolen parameter "add"
   indicating whether to create a new plot or add to an existing plot device.
* rbindList() has a new boolean parameter "keepNA" indicating whether to keep
   NA values in the results, which ultimately causes NA to be converted to
   "NA".
* makeNames() has a new boolean parameter "keepNA" indicating whether to keep
   NA values in the results. When keepNA is TRUE, NA is converted to "NA",
   otherwise NA entries are treated as "" prior to creating names.
* rmNA() changed the default value for parameter "rmNAnames" to FALSE, which
   was the more anticipated behavior. An input vector will not be shortened
   for non-NA values that have an NA name. Changing function parameter
   defaults will be an extremely rare occurrence in future.
* make_styles() which is a vectorized wrapper for `crayon::make_style()`,
   gains some finer control over color chroma and luminance.
* jargs() gets a larger refactoring, aimed at better display of list
   parameters, and lists of lists and other nested variations. It now does
   a better job of displaying the list and vector names appropriately.
   A new function `handleArgsText()` was split into a separate function, but
   is a rare non-exported function since it is currently only useful for
   `jargs()`.
* TODO.md was created to keep track of future enhancements and fixes.

## new functions

* applyLceiling() intended to restrict colors to a maximum luminance in HCL
   color space, mainly to support the new lightMode option intended for using
   printDebug() with a light background.
* checkLightMode() intends to support the new lightMode option by doing its
   best to check situations to enable lightMode. It first checks
   options("lightMode"). It then checks environment variables that suggest
   Rstudio is running, in which case it defaults to lightMode=TRUE, since
   the default Rstudio has a light background.
* sclass(), sdim(), and ssdim() are intended to help handle list objects.
   * sclass() returns the class of each list element.
   * sdim() returns the dimensions (or lengths) or each list element.
   * ssdim() is a special case that returns the dimensions of a list of
   list of objects.
   In all cases, if the input object is an S4 object, it operates on
   slotNames(x). Thus, calling ssdim(x) is helpful for S4 objects, since it
   returns the class and dimensions of each object inside the S4 object.

