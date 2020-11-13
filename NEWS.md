# jamba version 0.0.59.900

## changes to existing functions

* `plotSmoothScatter()` new argument `expand` will
expand the x-axis and y-axis ranges by this amount.
The default `expand=c(0.04, 0.04)` mimics the default
setting for R base plots. The help text was updated.
* `plotSmoothScatter()` now uses `grDevices::xy.coords()`
to define coordinates, avoiding internal logic that
essentially did the same thing. This change should be
silent. This change does remove the warnings that
were produced with input supplied as a single numeric vector.

## bug fixes

* `plotSmoothScatter()` argument `binpi` was setting the
bins per inch identically for x and y axes, using the
maximum size -- when it can and now does handle each axis
independently. Now the output pixels should be square,
rather than being influenced by the ratio of the plot
device.

# jamba version 0.0.58.900

## changes to existing functions

* `getColorRamp()` was updated for single-color input
and `defaultBaseColor`. It now properly creates a
color gradient from light-to-dark, or dark-to-light
based upon the relative brightness of `color` and
`defaultBaseColor`.

# jamba version 0.0.57.900

## changes to existing functions

Updates to `setCLranges()` mainly affect colorized console
text output, used by `printDebug()` which calls `make_styles()`.
This function is also useful to adjust a color or vector of
colors for contrast on a light or dark background.

* `setCLranges()` argument has new default `lightMode=NULL` and
slightly different, more intuitive behavior. When `lightMode` is
defined (`TRUE` or `FALSE`) then it over-rides existing values,
and uses the corresponding `lightMode` setting directly.
Otherwise, when `lightMode=NULL` any existing `Crange` and
`Lrange` values are used. If none are defined, then
`checkLightMode()` is called, then default values are used.
* Functions that use `setCLranges()` were updated so the
argument default `lightMode=NULL` is used. When those functions
are called with `lightMode=TRUE` or `lightMode=FALSE` the
text output will be adjusted. For example:
```
printDebug("yellow", lightMode=FALSE)
printDebug("yellow", lightMode=TRUE)
```

Functions affected:
* `checkLightMode()`
* `applyCLrange()`
* `setCLranges()`
* `printDebug()`
* `make_styles()`
* `jargs()`

# jamba version 0.0.56.900

## bug fixes

* `printDebug()` failed on class `"package_version"` ultimately
because `unnestList()` could not unlist this object type. The
"package_version" class is a list, e.g. `is.list(packageVersion("base"))`
is `TRUE`. However, accessing the first item, e.g. `x[[1]]`
always returns the full object unchanged... which is not expected
behavior for a list. This class was added to `stopClasses`.
* `unnestList()` was refactored to handle nested lists with
and without names in various combinations, and to use
`tryCatch()` as a final protection from infinite recursion,
when list-list objects do not behave like list objects
(as is the case with the `"package_version"` class.)
* `applyCLranges()` was updated to remove the warning message
for argument `fixYellow` that read
`"the condition has length > 1 and only the first element will be used"`.
The function now handles vector input for `fixYellow` and extends the
vector to the length of `x`.

## changes to existing functions

* `plotSmoothScatter()` now correctly determines the plot
aspect ratio after the plot is initiated. The situation occurs
when using `graphics::layout()` which does not update
the `par()` settings until the plot is created. Now a
blank plot is initiated before determining aspect ratio,
which allows proper calculation of the 2D density plot.

## refactoring of plotSmoothScatter

* `plotSmoothScatter()` new argument `binpi` defines the `nbin`
number of displayed bins based upon plot panel size. The default
`binpi=50` defines 50 display bins per inch. This value will
adjust accordingly for large and small figures, based upon
the physical size of the figure in inches.
* `plotSmoothScatter()` argument `nbin` new default is `nbin=NULL`,
which then uses `binpi` to calculate `nbin`. To use the previous
style, set `nbin=256` which will ignore `binpi`.
* `plotSmoothScatter()` new argument `bwpi` defines `bandwidthN`
based upon the plot panel size. The default `bwpi=NULL` does
not implement this scaling by default, instead `bandwidthN=300`
is still the default. The benefit of using `bwpi` is mostly
in speed of calculating 2D density for multi-panel plots
where detail would already be lost during display.

### plotSmoothScatter commentary

`plotSmoothScatter()` uses two arguments to determine how
data is plotted:

* `bandwidthN` defines the number of bandwidth
divisions on the x- and y-axes used to calculate the 2D density.
Higher `bandwidthN` calculates greater
detail in the 2D density -- these values are all typically higher
than `graphics::smoothScatter()`, and this is still the driving
reason to use `plotSmoothScatter` and not `smoothScatter()`.
* `nbin` defines the number of visible pixels used to display
the calculated 2D density. Higher `nbin` creates visually
more detailed representation of the 2D density.

Usually `nbin` and `bandwidthN` values are similar to each other,
since it conceptually makes sense to calculate 2D density at
roughly the resolution used to display the results.
These arguments are adjustable
and are still valid for most purposes.

So why the change? The short summary is that multi-panel plots
have fewer pixels being displayed, and I wanted a convenient method
to reduce the number of pixels (`nbin`) displayed in each panel.
(Truth be told, while working from home creating plots on remote servers,
I noticed just how long multi-panel plots take to render. Most of that
time is spent rendering detail never seen during display.)

After some extended testing, I concluded that bandwidth should
remain relatively constant regardless of the display pixels,
in order to maintain consistent 2D density despite the image size.
(Conceptually, it makes sense that the image display size should
not affect underlying calculation of the data to be displayed.)
However, the number of displayed pixels should be reduced roughly
proportional to the size of the plot panel, which solves the
issue I was having of plotting data with far more detail than
could be visually rendered, making plots larger, take longer
to display, and creating larger saved file sizes.

Also, the resolution of the resulting plot can now be adjusted
relative to the output plot size, consistent with computer
monitor resolution (dots per inch dpi), and printed paper output
dpi.


# jamba version 0.0.55.900

## changes to existing functions

* `applyXlsxConditionalFormat()` and `writeOpenxlsx()` were updated
to make default colors in `numStyle`, `intStyle`, `pvalueStyle`
slightly more colorful, while also being less dark, so Excel
default black text has better visible contrast. (Why can Excel
not apply conditional format rules to text font color?)
* `writeOpenxlsx()` format for P-values `pvalueFormat` formats
values above 0.01 as decimal value such as `0.022` or `0.157`,
P-values below 0.01 are exponential notation such as `2.31E-10`.
(I wish R could avoid formatting things like `1.1e+00` and
`1.0e+01` without using a custom function.)
* `writeOpenxlsx()` was updated with a more comprehensive example.
* `applyCategoricalFormat()` has a simple working example.
* `applyXlsxConditionalFormat()` has a simple working example.

## new functions

* `set_xlsx_rowheights()` and `set_xlsx_colwidths()` are light
wrappers to the very useful functions `openxlsx::setRowHeights()`
and `openxlsx::setColWidths()`. They simply open the workbook,
apply the values to the appropriate sheet, then save the workbook.

# jamba version 0.0.54.900

## changes to existing functions

* `setPrompt()` by default wraps the color sequence inside proper
ansi escape sequences, which helps the console properly apply
word wrapping by ignoring color sequences in the estimate of
character count per line.

# jamba version 0.0.53.900

## changes to existing functions

* `printDebug()` fixed a visual glitch caused by using `gsub()` to
remove trailing `sep` characters from vector elements before adding
the `sep` itself, without removing ANSI-encoding from the `sep`
string, sometimes removing trailing zero from numeric strings
but only in the middle of a multi-element vector.
Now the `sep` is appended only using `paste0()` and no trailing
`sep` characters are checked nor removed. This behavior is probably
better since it should properly represent the input content.
* `printDebug()` was modified to fix a longstanding visual glitch
that would not fully reset inverted ANSI colors that span multiple lines.
If the output uses color, a trailing `crayon::reset("")` is added to
the end of each line.

# jamba version 0.0.52.900

## new functions

* `jam_rapply()` is a lightweight customization to `base::rapply()`,
specifically designed to keep `NULL` entries without dropping them
silently. It has the added benefit of being able to flatten a nested
input list, then expand back into the original nested list structure,
without losing the `NULL` entries.

## Notes on sorting lists with factors and vectors

A theme of several updates this version has been to handle list
input that may contain `factor` and non-`factor` vectors.
In fact even handling a list of `factor` vectors is unclear
at least to me. For example, `base::unlist()` operating on a
list of factors will define one factor vector whose levels
are literally the unique first-occurrence of each factor level.
For example `sort(factor(c("A","B"), levels=c("A","B")))` results
in `c("A", "B")`, while `sort(factor(c("A","B"), levels=c("B","A")))`
results in `c("B", "A")`. So what should be done when given:

>`list(factor(c("A","B"), levels=c("A","B")), factor(c("A","B"), levels=c("B","A"))`

In reality, most such occurrences would not happen, if factor levels
are specified, the best solution is to include the full set of factor
levels with each factor vector. But if the input contains a mixture
of factor and character, vectors, running `unlist()` converts factors
to integers... which is really bad. At the very least I had to
handle and prevent that scenario.

## changes to existing functions

* `pasteByRowOrdered()` was modified in two ways: new argument
`na.last=TRUE` is passed to `mixedSortDF()` to allow `NA` values
to be sorted at the top, which seems intuitive for most scenarios;
also the `...` additional arguments are passed to `mixedSortDF()`
which allows column sort order to be customized. Previously
`pasteByRowOrdered()` called `mixedSortDF()` which sorted columns
in the order they appear in the `data.frame`, however sometimes
it is better to sort columns differently, while keeping the original
column order intact. See examples for illustration of `na.last=TRUE`.
In a two-column table with `NA` values in the second column,
using `na.last=TRUE` (default) would yield factor levels
`c("A_B", "A", "B_C", "B")`, while using `na.last=FALSE` would
yield `c("A", "A_B", "B", "B_C")`. This output seems more intuitive,
however the default in `mixedSortDF()` and `mixedSort()` are
`na.last=TRUE` so that default will be maintained for consistency.
* `mixedSort()` argument `NAlast` is deprecated in favor of
`na.last` for consistency with `base::sort()`.

## Bug fixes

* `cPaste()` was refactored to simplify internal logic, and to
handle a rare edge case that was inconsistent with related functions.
It occurs when
the input list contains some `factor` vectors, and some non-`factor`
vectors, and when both `doSort=TRUE` and `keepFactors=TRUE`.
The `keepFactors=TRUE` argument tries to maintain factor levels
during the sort, instead of using `mixedSort()` across all vectors.
In this case, previously the non-`factor` vectors were converted
to factor, which by default would order factor levels using
`sort()`. Now the output is consistent in that the `character`
vectors are converted to factors whose levels are ordered using
`mixedSort()`. The step occurs once across all `character` vectors,
so it is fairly fast even for large input lists. Also, any `NULL`
entries are still maintained and returned as `""`.
* `mixedSorts()` was updated to allow input that has no names,
which was errantly returning `NULL`. Now names are handled gracefully,
allowing no names, or even duplicated names, without issue.
* `mixedSorts()` now properly handles nested list input that
may contain `NULL`, so that it returns the same structured list
including `NULL` entries where appropriate.

## Refactoring for performance

* `uniques()` new argument `useSimpleBioc=TRUE` will optionally
enable Bioconductor SimpleList intermediate -- mostly available
to benchmark because it is so painfully slow it raised a red flag.
The Bioconductor `S4Vectors` package changed how they handle `List`
coersion from a simple `list` class, instead of using `CompressedList`
it defaulted to use `SimpleList`. Unfortunately, `SimpleList`
is roughly 6-10 times slower when calling `unique()`, and was causing
curious slowdowns in other functions such as `cPasteU`. Bioconductor
does not allow coersion directly to `CompressedList`, instead requires
coersion by specific recognized classes such as `"character"` with
`"CharacterList"`, and `"integer"` with `"IntegerList"`. So because
the input list is allowed to contain different classes, the remedy
is to make a `CompressedList` for each recognized class, call
`uniques()` on that `CompressedList`, and repeat until all classes
are handled. Any unrecognized classes are handled using
`lapply(x, unique)`. See examples for some benchmark comparisons.
When `useBioc=TRUE`, the optimized Bioconductor method is used.
When `useBioc=FALSE`, and `useSimpleBioc=TRUE`, the slower
Bioconductor approach is used, which uses `SimpleList` as an
intermediate.
When both `useBioc=FALSE` and `useSimpleBioc=FALSE`, there are two
additional options:
`keepNames=FALSE` uses `lapply(x, unique)` and loses vector names;
`keepNames=TRUE` will invoke a special base R approach that maintains the
original vector names for the remaining unique entries.
Of these four approaches the timings relative to the fastest:
1. useBioc=TRUE: 1x (fastest)
2. useBioc=FALSE, useSimpleBioc=TRUE: 6x slower
3. useBioc=FALSE, useSimpleBioc=FALSE, keepNames=FALSE: 1.8x slower
4. useBioc=FALSE, useSimpleBioc=FALSE, keepNames=TRUE: 12x slower

# jamba version 0.0.51.900

## changes to existing functions

* `printDebug()` has a new argument, largely silent, `doReset`
which defines whether to apply an additional color reset to
the delimiter argument `sep`. Previously the delimiter inherited
the color from the previous text, but now by default the delimiter
style is reset, giving more clear distinction between multiple
values.
* `printDebug()` now properly applies the Crange and Lrange values
when lightMode is TRUE, which means it limits the brightness of
text colors on a light background, to ensure enough contrast to
read the text. When using `printDebugI()` the background color
is not adjusted, and the foreground (text) color is set using
`setTextContrastColor()`, meaning either white or black depending
upon the background brightness.
* `setCLranges()` and `make_styles()` were updated to apply
options `"jam.Crange"`, `"jam.Lrange"` only when those options were
`NULL`, otherwise leave them as-is.
* `applyCLrange()` now properly applies the desired `Crange` and `Lrange`
values, and defines the appropriate options, by default only
when those options are `NULL`.
* `fixYellow()` default settings were changed to expand the range
corrected "for green-ness", also `fixup=TRUE` by default.
* `hcl2col()` argument was changed to `fixup=TRUE`, and `model="hcl"`,
consistent with other changes in color handling.
* `hcl2col()` and `col2hcl()` now use `model="hcl"` by default,
which now calls the `farver` package if installed, otherwise they
revert to using colorspace `model="polarLUV"` and `fixup=TRUE`. In
future colorspace will likely be removed in favor of farver, so
for now the transition has begun.
* `setTextContrastColor()` was adjusted to handle alpha transparency
more accurately, the examples were updated and include a new example
that uses `drawLabels()` on a dark background. New argument `alphaLens`
allows some fine-tuning of the alpha transparency blending of
foreground and background colors.

## Other updates

Added some visuals to README.Rmd.

# jamba version 0.0.50.900

## changes to existing functions

* `shadowText()` was updated to use several `options()` as default
argument values. Although `options()` is not always an ideal mechanism,
for purely visual effects it is not in conflict with statistical
reproducibility, and allows some overall graphical settings to be
maintained when `shadowText()` is being called through internal
`plot()` functions. For example setting `text <- jamba::shadowText`
then calling a default base R plot function will use the
`jamba::shadowText()` in place of `text()`, but without allowing
any customization. The `options()` mechanic allows some custom settings
to be honored. The primary driver for these changes is in
`multienrichjam::jam_igraph()` which plots `igraph` objects with
some customization, one option is `use_shadowText=TRUE` which
invokes `jamba::shadowText()`. It is desirable to adjust the
contrast of the outline effect so it does not completely obscure
the underlying network graph display.

# jamba version 0.0.49.900

## New experimental function

* `fix_matrix_ratio()` is used to expand a matrix to have
roughly 1:1 dimensions, up to a maximum factor per dimension.
The purpose is to enhance `image()`, `rasterImage()`, and
`grid.raster()` functions, specifically when `interpolate=TRUE`
for images with more rows or columns than there are actual
pixels to display the image. The `interpolate=TRUE` methods
in base R do not account for assymetry, nor do they account
for a low number of rows or columns. See examples for visible
examples of the effect.

# jamba version 0.0.48.900

## Enhancements to README

The README.Rmd was re-organized, with more callouts of
helpful `jamba` functions. Next step will be to include
visual examples.

## Minor bug fix

* `writeOpenxlsx()` was exporting files with inconsistent word wrapping
by column, and inconsistent vertical align (`valign`) settings.
The culprit was `openxlsx::saveWorkbook()` which appears to apply
default styles for cell styles that are not specifically encoded.
Simply loading and saving a workbook would modify column styles.
The workaround/fix was to ensure `writeOpenxlsx()` would specifically
encode `wrapText` and `valign` for each cell, so that it would not
be overridden by `openxlsx::saveWorkbook()` mood.
* `writeOpenxlsx()` now forces `wrapText=TRUE` for all columns. In future
the function may allow per-column `wrapText`.
* `applyXlsxCategorical()` has new argument `wrapText=TRUE` which is passed
to `openxlsx::createStyle()`, since that function requires wrapText
be either `TRUE` or `FALSE`, and its default is `FALSE`.

# jamba version 0.0.47.900

## Enhancements

* `setTextContrastColor()` no longer calls `par("bg")` to
determine the background color, unless a graphics device is
already open. This change will prevent a graphics device from
being opened in somewhat random situations. The argument `bg`
is only used when input `colors` have alpha transparency.
When `bg` is not supplied, it will query `par("bg")` only
when `length(dev.list())>0`, otherwise it uses `"white"`
by default.
Note that calling `par()` always opens a new graphics device
if one is not already open, even though the `"bg"` value is
stored somewhere and should not require a new device. The R
docs do describe this behavior, for what it's worth.
* `imageByColors()` now accepts argument `cellnote` as an
atomic vector, which is coerced to a matrix with same
dimensions as input matrix `x`. This change makes it easier
to use numeric formatting functions, which sometimes take
matrix input and provide vector output.
* `applyCLrange()` new argument `fixup` which is passed to
`hcl2col()` which in turn passes it to `colorspace::hex()`
when converting colors outside the color gamut. These functions will
soon be replaced with calls to `farver`.
* `hcl2col()` and `col2hcl()` now have argument option `model="hcl"` 
that uses farver, and its color model `"hcl"`, which is equivalent
to using `colorspace::hex(...,fixup=TRUE)`, except faster,
and with better upside for other color manipulations. For now,
the color model will not use `"hcl"` until I can work out better
default values in `colorjam::rainbowJam()`.

# jamba version 0.0.46.900

## Enhancements

* `rbindList()` has two optional arguments: 

# jamba version 0.0.45.900

## new functions

* `sdima()` and `ssdima()` are equivalent to `sdim()` and
`ssdim()` except that they operate on `attributes(x)`. I somehow
found myself constantly typing `sdim(attributes(x))` and finally
had enough of that.
* `printDebugI()` is a shortcut to `printDebug(..., invert=TRUE)`,
intended to be a quick way to use inverted colors, to colorize
the background instead of the text. Note that `printDebug()` applies
a color range restriction, to make sure the text contrasts with
either light or dark background. When `invert=TRUE` the background
color is not restricted, instead the foreground text uses
`setTextContrastColor()` to determine a suitable contrasting
color for the text and its corresponding background color.

## changes to existing functions

* `imageByColors()` was refactored to handle grouped labels more
cleanly. New argument `groupByColors=TRUE` decides whether to
group cellnotes only when the label and underlying color
are both identical -- which is a change from previous behavior.
Previous behavior grouped consecutive identical labels, regardless
the underlying color, which is sometimes helpful, for example
labeling a color gradient by the base color.
Typically the cellnote and color would change together, but not
always. The new default is consistent with the expected behavior.
* `imageByColors()` new argument `groupBy` with one or both values
`"row"` which groups cellnote values by row, and `"column"` which
groups cellnote values by column. Useful to limit cellnote grouping
so the values are not grouped inappropriately. This argument is
mostly useful when `groupByColors=FALSE`.
* `imageByColors()` new argument `adjustMargins=TRUE` will call
`adjustAxisLabelMargins()` to adjust the margins to ensure the labels
will fit in the plot device size.
* `adjustAxisLabelMargins()` was updated to handle `par("cex")` and
`par("cex.axis")` appropriately.

# jamba version 0.0.44.900

## enhancements

* `sdim()` and `ssdim()` now handle environment input, or list of
environments. Previously worked for `sdim()` but using random order
returned by `names(x)` instead of proper order from `ls(envir=x)`.
An environment is essentially treated as a list, and `ssdim()` will
now properly traverse into the environment as if it were a list.
* `sdim()` and `ssdim()` now handle `"List"` object classes and
sub-classes from the `S4Vectors` Bioconductor package, but it
only works when the `S4Vectors` package is previously loaded
into the search path, otherwise the R object object system
does not load the dependent package automatically. In that case,
the `slotNames(x)` are used, which is somewhat less useful.
* `imageDefault()` now properly honors the `par("bty")` setting,
that is by not calling its own `box()` to force drawing a
box around the heatmap.

# jamba version 0.0.43.900

## bug fixes / enhancements

* `make_styles()` fixed issue with background color supplied in vector
form, it was errantly calling `fixYellow()` with the logical vector
of whether to colorize, instead of the color vector itself. This
package needs `"testthat"`.

# jamba version 0.0.42.900

## bug fixes / enhancements

* `printDebug()` now handles nested lists, calling `jamba::unnestList()`
which flattens nested lists to one layer. This change allows
printing a list `printDebug(c("one", "two "), c("three", "four"))`
or `l <- list(c("one", "two "), c("three", "four"));printDebug(l);`.
* `printDebug()` now uses `color2gradient()` to create alternating
light-dark shadings, used when a single color is defined for
a multi-item vector concatenated by `sep=","`. The gradient
was more reliable (so far) than `makeColorDarker()` because
ANSI output is limited, as are the colors allowed for contrast
with light or dark background.
* `printDebug()` new argument `htmlOut=TRUE` will output character
string containing text colorized using HTML using the format
`<span style="color:red;background=white">text</span>`. Intended
whenever the colorized text would not otherwise be interpreted
and colorized in a web browser context. It calls `make_html_styles()`.
* `make_html_styles()` is a new function, which takes a vector
of text, a vector of foreground colors, a vector of background colors,
and returns a character string with HTML which colorizes the text.
This function is intended for Rmarkdown or web page HTML output.
* `jargs()` was updated to handle arguments with vectors containing
negative numbers. The negative `-` sign for example from `-3` is
being returned by `formals()` as `call(\\`-\\`, 3)`, which
stores the numeric value separate from the negative sign. This
seems like a change, which therefore broke how the colorization
previously worked. For now the function works, but somehow
lost the ability to colorize numeric vectors by value. Will
leave for future.


# jamba version 0.0.41.900

## bug fixes

* `pasteByRow()` properly handles leading blanks, it
previously only correctly handled blank values in
the second and subsequent columns. Caused by trying
to simplify the R function when moving into the jamba
package.

# jamba version 0.0.40.900

## changes

* `plotPolygonDensity()` new argument `highlightPoints`
allows highlighting points in the distribution, which
is currently displayed as histogram bars on top of
the histogram and density polygon fill. Also 
slight change to how the polygon density is sized,
it uses the median ratio of the height
of the density curve to the histogram bars at the center
of each bar.

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

## bug fixes

* Fixed regression in `provigrep()` when input is not character,
the use of `make.unique()` requires explicit conversion to character
type first.

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

