## TODO for jamba

* Add message during package install suggesting the "crayon" package be
installed if not already installed.

### Bug fixes / Enhancements to existing functions

1. `plotSmoothScatterG()` is a new function which would create a
smooth scatter plot like `plotSmoothScatter()` except that it will
also allow coloring points by group. The previous closest estimate
was the ability to overlay contours of different colors, however
the transparency of each layer was not effective, mostly letting
the last color drawn become the dominant color. This function must
calculate two things: the blended color defined by the composition
of points in each cell, and the intensity of that blended color
based upon the total number of points per cell. All attempts at blending
gradient colors has been problematic, since for example three half-tone
colors blend together to one half-tone grey color, which loses the fact
that there might be a large number of points in the cell, and should
therefore be darkly colored.

2. `printDebug()` option for HTML or Rmarkdown-friendly output.

3. `jargs()` is choking on certain argument formats:

    * `jargs(writeOpenxlsx, "lfcRule")` displays the escaped ANSI
    color-codes instead of applying the color-codes for negative values.
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


### printDebug color range updates

* Something in the transition from RGB to sRGB in the colorspace
package caused the color ranges (lightMode on and off) to be
slightly off. Also `setTextContrastColor()` needs a slight tweak
to use dark text more often.


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

* optinoal recursive operation, in the event of a nested list of data.frames,
it should recurse through the list, calling rbindList() on each list element
and progressive build up one resulting data.frame or matrix.


### imageByColors

* optional boolean parameter to transpose the image, i.e. t(x) and
   if applicable t(cellnote).
* optionally draw boxes around grouped labels, a visual indicator of
   groups of cells sharing one label. Care should be taken not to enable
   this functionality with a large table containing no grouped labels, or
   even with any scenario resulting in "too many boxes".

### color brightness and saturation handling

* consider adding helper functions like darken(), lighten(), saturate(),
   desaturate(). Note colorspace::desaturate() completely removes all color
   saturation (chroma), and conflicts with this function naming scheme.
   We could use jamdesaturate(), jamdarken(), jambrighten(), jamsaturate().
* add new function subsetColors() which internally creates a data.frame with
   hex, RGB, HSV, and HCL values, which can then be used to subset an input
   set of colors. Bonus points for accepting different color classes at
   input, and returning the same color class at output.

