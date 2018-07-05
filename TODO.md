## TODO for jamba

### Functions to add

* cPaste(), cPasteUnique() -- requires S4Vectors from Bioconductor, which
should be a "Suggests" entry with conditional fallback to cover unstrsplit().
When adding Bioconductor to DESCRIPTION, the line "biocViews:" needs to appear
with nothing in that section. cPaste() takes a list of vectors, and returns
a delimited character vector, usually separated by commas. It optionally
sorts entries using mixedSort(), optimized for the whole vector instead
of sorting each list element individually. cPasteUnique() is the same,
except it applies unique() to each list element before calling cPaste(),
and is also optimized across the whole list.

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

* optinoal recursive operation, in the event of a nested list of data.frames,
it should recurse through the list, calling rbindList() on each list element
and progressive build up one resulting data.frame or matrix.

### mixedSortDF

* allow byCols to accept colnames, optionally with prefix "-"
   which should be handle by reversing the sort order for that
   column.

### imageByColors

* optional boolean parameter to transpose the image, i.e. t(x) and
   if applicable t(cellnote).
* optional boolean parameter to flip the y-axis, which otherwise appears
   upside-down compared to how one envisions a table layout.
* optionally draw boxes around grouped labels, a visual indicator of
   groups of cells sharing one label. Care should be taken not to enable
   this functionality with a large table containing no grouped labels, or
   even with any scenario resulting in "too many boxes".

### color brightness and saturation handling

* consider replacing applyLceiling() with applyCLrange(), with options
   to force colors within either L (lumina, lightness) range, and/or within
   C (chroma, coloration) range.
* consider adding helper functions like darken(), lighten(), saturate(),
   desaturate(). Note colorspace::desaturate() completely removes all color
   saturation (chroma), and conflicts with this function naming scheme.
   We could use jamdesaturate(), jamdarken(), jambrighten(), jamsaturate().
* add new function subsetColors() which internally creates a data.frame with
   hex, RGB, HSV, and HCL values, which can then be used to subset an input
   set of colors. Bonus points for accepting different color classes at
   input, and returning the same color class at output.

### jargs

* issue where function parameters are named lists, the names of each list
are not displayed, for example jargs(doNorm).
* FIXED: issue where the grep pattern matches no parameter names, and results
in an error instead of simply returning a notice that no parameter
names match the grep pattern.
