#' jamba: Jam Base Methods
#'
#' The jamba package contains several jam base functions
#' which are re-usable for routine R analysis work, and are
#' important dependencies for other Jam R packages.
#'
#' See the function reference for a complete list of functions.
#'
#' The goal is to implement methods as lightweight as possible, so
#' so inclusion in an analysis workflow will not incur
#' a noticeable burden.
#'
#' @section plot functions:
#'    * [plotSmoothScatter()] smoothScatter() enhanced for more visual detail
#'    * [imageDefault()] enhanced rasterized image() with fixed aspect ratio
#'    * [imageByColors()] for `data.frame` of colors and optional
#'    labels centered across repeated values.
#'    * [showColors()] color display for vector, list, color function,
#'    or mixed formats.
#'    * [nullPlot()] blank plot that labels the current margin sizes
#'    * [minorLogTicksAxis()] log-scale axis ticks in base R with
#'    custom log base, optional offset, e.g. `log2(1 + x)`
#'    * [shadowText()] base R text labels with shadow or outline or both,
#'    also [shadowText_options()].
#'    * [getPlotAspect()], [decideMfrow()] convenience base R graphics.
#'
#' @section string functions:
#'    * [mixedSort()], [mixedOrder()], [mixedSortDF()] - efficient
#'    alphanumeric "version" sort, with options helpful for gene symbols.
#'    * [vgrep()], [vigrep()], [igrep()], [vigrep()] fast grep wrappers
#'    for value-return, case-insensitive search.
#'    * [provigrep()], [proigrep()] - progressive, ordered grep to use
#'    pattern matching to re-order a vector.
#'    * [makeNames()] create unique, versioned names with custom format
#'    * [nameVector()] apply names to vector dynamically
#'    * [nameVectorN()] vector of named names useful with [lapply()].
#'    * [pasteByRow()], [pasteByRowOrdered()] paste data.frame and matrix
#'    values by row, skipping blanks, optional factor order.
#'    * [rbindList()] convert list to `matrix` or `data.frame`.
#'    * [tcount()] extends `table()` to sort by size and optional minimum
#'    count filter.
#'
#' @section color functions:
#'    * [rgb2col()], [col2hcl()], [col2hcl()], [col2hsv()], [hsv2col()] color
#'    interconversion
#'    * [setTextContrastColor()] text contrast color per given background color
#'    * [getColorRamp()] catch-all to get named gradients, or expand
#'    one or more colors to gradient.
#'    * [makeColorDarker()], [color2gradient()], [showColors()] color
#'    manipulation and display
#'
#' @section miscellaneous helper functions:
#'    * [printDebug()] colored text output to console, 'Rmarkdown', HTML
#'    * [kable_coloring()] colored `kableExtra::kable()` output for 'Rmarkdown'
#'    * [setPrompt()] colored R prompt
#'    * [deg2rad()], [rad2deg()] interconvert degrees to radians.
#'    * [getDate()], [asDate()], [dateToDaysOld()] human-readable,
#'    opinionated date formatting
#'    * [padString()], [padInteger()] pad character or integer strings
#'    * [rmNA()], [rmNULL()], [rmInfinite()] remove or replace missing or NA
#'    values with defined alternatives
#'
#' @section export and import functions:
#'    * [readOpenxlsx()] import worksheets from 'xlsx' 'Excel' files.
#'    * [writeOpenxlsx()] export worksheets to 'xlsx' 'Excel' files with color,
#'    formatting, and styling.
#'
#' @section Jam options:
#'    The `jamba` package recognizes some global options, but limits these
#'    options to include only non-analysis options. For example, no global
#'    option should change the numerical manipulation of data.
#'    * `jam.lightMode` - `logical` whether the R console or graphical
#'       background is light or dark, `printDebug()` limits
#'       the luminance range to maximize visual contrast.
#'    * `jam.Crange`,`jam.Lrange` - numerical values used by `printDebug()`
#'    to maximize visual contrast, used with `jam.lightMode`.
#'    * `jam.shadowColor`,`jam.shadow.r`,`jam.shadow.n`,`jam.alphaShadow`,
#'    `jam.outline`,`jam.alphaOutline` to customize details for `shadowText()`,
#'    see `shadowText_options()` for convenience.
#'
#' @keywords internal
"_PACKAGE"


## usethis namespace: start
#' @importFrom methods as
#' @importFrom utils head
#' @importFrom utils tail
## usethis namespace: end
NULL
