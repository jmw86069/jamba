# Package index

## mixedSort functions

Functions based upon alphanumeric sorting

- [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md)
  : order alphanumeric values keeping numeric values in proper order
- [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  : sort alphanumeric values keeping numeric values in proper order
- [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
  : sort data.frame keeping numeric values in proper order
- [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)
  : sort alphanumeric values within a list format
- [`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md)
  : order alphanumeric values from a list

## Grep functions

Convenient grep functions for speed of re-use

- [`grepls()`](https://jmw86069.github.io/jamba/reference/grepls.md) :
  Search for objects in the environment
- [`igrep()`](https://jmw86069.github.io/jamba/reference/igrep.md) :
  case-insensitive grep
- [`igrepHas()`](https://jmw86069.github.io/jamba/reference/igrepHas.md)
  : vector contains any case-insensitive grep match
- [`igrepl()`](https://jmw86069.github.io/jamba/reference/igrepl.md) :
  case-insensitive logical grepl
- [`provigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md)
  [`proigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md)
  : provigrep: progressive case-insensitive value-grep
- [`unigrep()`](https://jmw86069.github.io/jamba/reference/unigrep.md) :
  case-insensitive grep, returning unmatched indices
- [`unvigrep()`](https://jmw86069.github.io/jamba/reference/unvigrep.md)
  : case-insensitive grep, returning unmatched values
- [`vgrep()`](https://jmw86069.github.io/jamba/reference/vgrep.md) :
  grep, returning values
- [`vigrep()`](https://jmw86069.github.io/jamba/reference/vigrep.md) :
  case-insensitive grep, returning values

## Plot functions

Functions that assist with R plotting

- [`adjustAxisLabelMargins()`](https://jmw86069.github.io/jamba/reference/adjustAxisLabelMargins.md)
  : Adjust axis label margins
- [`coordPresets()`](https://jmw86069.github.io/jamba/reference/coordPresets.md)
  : Process coordinate adjustment presets
- [`decideMfrow()`](https://jmw86069.github.io/jamba/reference/decideMfrow.md)
  : Decide plot panel rows, columns for graphics::par(mfrow)
- [`drawLabels()`](https://jmw86069.github.io/jamba/reference/drawLabels.md)
  : Draw text labels on a base R plot
- [`getPlotAspect()`](https://jmw86069.github.io/jamba/reference/getPlotAspect.md)
  : Get aspect ratio for coordinates, plot, or device
- [`groupedAxis()`](https://jmw86069.github.io/jamba/reference/groupedAxis.md)
  : Draw grouped axis labels
- [`imageByColors()`](https://jmw86069.github.io/jamba/reference/imageByColors.md)
  : Display color raster image using a matrix of colors
- [`imageDefault()`](https://jmw86069.github.io/jamba/reference/imageDefault.md)
  : Display a color raster image
- [`minorLogTicksAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md)
  [`logFoldAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md)
  [`pvalueAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md)
  : Display major and minor tick marks for log-scale axis
- [`nullPlot()`](https://jmw86069.github.io/jamba/reference/nullPlot.md)
  : Create a blank plot with optional labels
- [`plotPolygonDensity()`](https://jmw86069.github.io/jamba/reference/plotPolygonDensity.md)
  : Plot distribution and histogram overlay
- [`plotRidges()`](https://jmw86069.github.io/jamba/reference/plotRidges.md)
  : Plot ridges density plots for numeric matrix input
- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  : Smooth scatter plot with enhancements
- [`shadowText()`](https://jmw86069.github.io/jamba/reference/shadowText.md)
  : Draw text with shadow border
- [`shadowText_options()`](https://jmw86069.github.io/jamba/reference/shadowText_options.md)
  : Get and set options for shadowText
- [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md)
  : Show colors from a vector or list
- [`sqrtAxis()`](https://jmw86069.github.io/jamba/reference/sqrtAxis.md)
  : Determine square root axis tick mark positions
- [`usrBox()`](https://jmw86069.github.io/jamba/reference/usrBox.md) :
  Draw colored box indicating R plot space

## Numeric functions

Functions which manipulate numeric data

- [`deg2rad()`](https://jmw86069.github.io/jamba/reference/deg2rad.md) :
  Convert degrees to radians
- [`noiseFloor()`](https://jmw86069.github.io/jamba/reference/noiseFloor.md)
  : Apply noise floor and ceiling to numeric vector
- [`normScale()`](https://jmw86069.github.io/jamba/reference/normScale.md)
  : Scale a numeric vector from 0 to 1
- [`rad2deg()`](https://jmw86069.github.io/jamba/reference/rad2deg.md) :
  Convert radians to degrees
- [`rowGroupMeans()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md)
  [`rowGroupRmOutliers()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md)
  : Calculate row group means, or other statistics
- [`rowRmMadOutliers()`](https://jmw86069.github.io/jamba/reference/rowRmMadOutliers.md)
  : Remove outlier points per row by MAD factor threshold
- [`warpAroundZero()`](https://jmw86069.github.io/jamba/reference/warpAroundZero.md)
  : Warp a vector of numeric values relative to zero

## String functions

Functions which sort or manipulate character strings

- [`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md) :
  convert numeric value or R object to human-readable size
- [`breaksByVector()`](https://jmw86069.github.io/jamba/reference/breaksByVector.md)
  : break a vector into groups
- [`fillBlanks()`](https://jmw86069.github.io/jamba/reference/fillBlanks.md)
  : Fill blank entries in a vector
- [`formatInt()`](https://jmw86069.github.io/jamba/reference/formatInt.md)
  : Format an integer as a string
- [`gsubOrdered()`](https://jmw86069.github.io/jamba/reference/gsubOrdered.md)
  : Global substitution into ordered factor
- [`gsubs()`](https://jmw86069.github.io/jamba/reference/gsubs.md) :
  Pattern replacement with multiple patterns
- [`makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md)
  : make unique vector names
- [`nameVector()`](https://jmw86069.github.io/jamba/reference/nameVector.md)
  : assign unique names for a vector
- [`nameVectorN()`](https://jmw86069.github.io/jamba/reference/nameVectorN.md)
  : define a named vector using vector names
- [`padInteger()`](https://jmw86069.github.io/jamba/reference/padInteger.md)
  : prefix integers with leading zeros
- [`padString()`](https://jmw86069.github.io/jamba/reference/padString.md)
  : pad a character string to a fixed length
- [`pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md)
  : Paste data.frame rows into character vector
- [`pasteByRowOrdered()`](https://jmw86069.github.io/jamba/reference/pasteByRowOrdered.md)
  : Paste data.frame rows into an ordered factor
- [`sizeAsNum()`](https://jmw86069.github.io/jamba/reference/sizeAsNum.md)
  : convert size to numeric value
- [`tcount()`](https://jmw86069.github.io/jamba/reference/tcount.md) :
  frequency of entries, ordered by frequency
- [`ucfirst()`](https://jmw86069.github.io/jamba/reference/ucfirst.md) :
  Uppercase the first letter in each word

## List functions

Functions which manipulate list objects

- [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md)
  [`cPasteS()`](https://jmw86069.github.io/jamba/reference/cPaste.md)
  [`cPasteSU()`](https://jmw86069.github.io/jamba/reference/cPaste.md)
  [`cPasteUnique()`](https://jmw86069.github.io/jamba/reference/cPaste.md)
  [`cPasteU()`](https://jmw86069.github.io/jamba/reference/cPaste.md) :
  paste a list into a delimited vector
- [`heads()`](https://jmw86069.github.io/jamba/reference/heads.md) :
  Apply head() across each element in a list of vectors
- [`jam_rapply()`](https://jmw86069.github.io/jamba/reference/jam_rapply.md)
  : Jam-specific recursive apply
- [`list2df()`](https://jmw86069.github.io/jamba/reference/list2df.md) :
  Convert list of vectors to data.frame with item, value, name
- [`mergeAllXY()`](https://jmw86069.github.io/jamba/reference/mergeAllXY.md)
  : Merge list of data.frames retaining all rows
- [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)
  : sort alphanumeric values within a list format
- [`rbindList()`](https://jmw86069.github.io/jamba/reference/rbindList.md)
  : rbind a list of vectors into matrix or data.frame
- [`relist_named()`](https://jmw86069.github.io/jamba/reference/relist_named.md)
  : relist a vector which allows re-ordered names
- [`rlengths()`](https://jmw86069.github.io/jamba/reference/rlengths.md)
  : lengths for recursive lists
- [`sclass()`](https://jmw86069.github.io/jamba/reference/sclass.md) :
  return the classes of a list of objects
- [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md)
  [`sdima()`](https://jmw86069.github.io/jamba/reference/sdim.md)
  [`ssdima()`](https://jmw86069.github.io/jamba/reference/sdim.md)
  [`ssdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) :
  print dimensions of list object elements
- [`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md) :
  apply unique to each element of a list
- [`unnestList()`](https://jmw86069.github.io/jamba/reference/unnestList.md)
  : Un-nest a nested list into a simple list

## Date functions

Functions which manipulate date objects or strings

- [`asDate()`](https://jmw86069.github.io/jamba/reference/asDate.md) :
  convert date DDmmmYYYY to Date
- [`dateToDaysOld()`](https://jmw86069.github.io/jamba/reference/dateToDaysOld.md)
  : convert date to age in days
- [`getDate()`](https://jmw86069.github.io/jamba/reference/getDate.md) :
  get simple date string

## Color functions

Functions that manipulate colors

- [`alpha2col()`](https://jmw86069.github.io/jamba/reference/alpha2col.md)
  : set R color alpha value
- [`applyCLrange()`](https://jmw86069.github.io/jamba/reference/applyCLrange.md)
  : Apply CL color range
- [`col2alpha()`](https://jmw86069.github.io/jamba/reference/col2alpha.md)
  : get R color alpha value
- [`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md) :
  convert R color to HCL color matrix
- [`col2hsl()`](https://jmw86069.github.io/jamba/reference/col2hsl.md) :
  convert R color to HSL color matrix
- [`col2hsv()`](https://jmw86069.github.io/jamba/reference/col2hsv.md) :
  Convert R color to HSV matrix
- [`color2gradient()`](https://jmw86069.github.io/jamba/reference/color2gradient.md)
  : Make a color gradient
- [`fixYellow()`](https://jmw86069.github.io/jamba/reference/fixYellow.md)
  : Fix yellow color
- [`fixYellowHue()`](https://jmw86069.github.io/jamba/reference/fixYellowHue.md)
  : Fix yellow color hue
- [`getColorRamp()`](https://jmw86069.github.io/jamba/reference/getColorRamp.md)
  : get color ramp by name, color, or function
- [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md) :
  convert HCL to R color
- [`hsl2col()`](https://jmw86069.github.io/jamba/reference/hsl2col.md) :
  convert HCL to R color
- [`hsv2col()`](https://jmw86069.github.io/jamba/reference/hsv2col.md) :
  Convert HSV matrix to R color
- [`isColor()`](https://jmw86069.github.io/jamba/reference/isColor.md) :
  detect valid R color
- [`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md)
  : Extend kableExtra colorization of 'Rmarkdown' tables
- [`makeColorDarker()`](https://jmw86069.github.io/jamba/reference/makeColorDarker.md)
  : make R colors darker (or lighter)
- [`rainbow2()`](https://jmw86069.github.io/jamba/reference/rainbow2.md)
  : Simple rainbow palette replacement
- [`rgb2col()`](https://jmw86069.github.io/jamba/reference/rgb2col.md) :
  Convert RGB color matrix to R color
- [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md)
  : Get Chroma and Luminance ranges for the given lightMode
- [`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md)
  : Define visible text color
- [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md)
  : Show colors from a vector or list
- [`unalpha()`](https://jmw86069.github.io/jamba/reference/unalpha.md) :
  Remove alpha transparency from colors
- [`warpRamp()`](https://jmw86069.github.io/jamba/reference/warpRamp.md)
  : Warp colors in a color ramp

## Export functions

Exporting data in xlsx format

- [`applyXlsxCategoricalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxCategoricalFormat.md)
  : Add categorical colors to 'Excel' 'xlsx' worksheets
- [`applyXlsxConditionalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxConditionalFormat.md)
  : Xlsx Conditional formatting
- [`readOpenxlsx()`](https://jmw86069.github.io/jamba/reference/readOpenxlsx.md)
  : Import one or more data.frame from 'Excel' 'xlsx' format
- [`set_xlsx_colwidths()`](https://jmw86069.github.io/jamba/reference/set_xlsx_colwidths.md)
  : Set column widths in Xlsx files
- [`set_xlsx_rowheights()`](https://jmw86069.github.io/jamba/reference/set_xlsx_rowheights.md)
  : Set row heights in Xlsx files
- [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
  : Export a data.frame to 'Excel' 'xlsx' format

## Practical functions

Practical functions to enhance routine R work, miscellaneous

- [`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md)
  : Calculate more detailed density of numeric values
- [`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md)
  : Safely call a function using ellipsis
- [`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md)
  : check lightMode for light background color
- [`check_pkg_installed()`](https://jmw86069.github.io/jamba/reference/check_pkg_installed.md)
  : Lightweight method to check if an R package is installed
- [`colNum2excelName()`](https://jmw86069.github.io/jamba/reference/colNum2excelName.md)
  : convert column number to 'Excel' column name
- [`color_dither()`](https://jmw86069.github.io/jamba/reference/color_dither.md)
  : Make dithered color pattern light-dark
- [`exp2signed()`](https://jmw86069.github.io/jamba/reference/exp2signed.md)
  : exponentiate log2 values with directionality
- [`getAxisLabel()`](https://jmw86069.github.io/jamba/reference/getAxisLabel.md)
  : Get axis label for minorLogTicks
- [`isFALSEV()`](https://jmw86069.github.io/jamba/reference/isFALSEV.md)
  : Vectorized isFALSE
- [`isTRUEV()`](https://jmw86069.github.io/jamba/reference/isTRUEV.md) :
  Vectorized isTRUE
- [`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md) :
  Show R function arguments jam-style
- [`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md)
  : Extend kableExtra colorization of 'Rmarkdown' tables
- [`lldf()`](https://jmw86069.github.io/jamba/reference/lldf.md) : Long
  listing of R session objects
- [`log2signed()`](https://jmw86069.github.io/jamba/reference/log2signed.md)
  : log2 transformation with directionality
- [`middle()`](https://jmw86069.github.io/jamba/reference/middle.md) :
  Return the middle portion of data similar to head and tail
- [`minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md)
  : Calculate major and minor tick marks for log-scale axis
- [`newestFile()`](https://jmw86069.github.io/jamba/reference/newestFile.md)
  : Return the newest file from a vector of files
- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  [`printDebugI()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  [`printDebugHtml()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  : Print colorized output to R console
- [`reload_qmd_cache()`](https://jmw86069.github.io/jamba/reference/reload_qmd_cache.md)
  : Reload Quarto 'qmd' cache
- [`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md)
  : Reload 'Rmarkdown' cache
- [`renameColumn()`](https://jmw86069.github.io/jamba/reference/renameColumn.md)
  : Rename columns in a data.frame, matrix, tibble, or GRanges object
- [`rmInfinite()`](https://jmw86069.github.io/jamba/reference/rmInfinite.md)
  : remove Infinite values
- [`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md) :
  remove NA values
- [`rmNAs()`](https://jmw86069.github.io/jamba/reference/rmNAs.md) :
  remove NA values from list elements
- [`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md) :
  remove NULL entries from list
- [`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)
  : set R prompt with project name and R version

## Heatmap functions

Specific enhancements for
[`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html)

- [`cell_fun_label()`](https://jmw86069.github.io/jamba/reference/cell_fun_label.md)
  : ComplexHeatmap cell function to label heatmap cells
- [`heatmap_column_order()`](https://jmw86069.github.io/jamba/reference/heatmap_column_order.md)
  : Return Heatmap column order from ComplexHeatmap heatmap object
- [`heatmap_row_order()`](https://jmw86069.github.io/jamba/reference/heatmap_row_order.md)
  : Return Heatmap row order from ComplexHeatmap heatmap object

## Internal functions

Functions typically for internal use by other Jam functions

- [`handleArgsText()`](https://jmw86069.github.io/jamba/reference/handleArgsText.md)
  : Handle function arguments as text
- [`jamCalcDensity()`](https://jmw86069.github.io/jamba/reference/jamCalcDensity.md)
  : Calculate scatter plot point density
- [`make_html_styles()`](https://jmw86069.github.io/jamba/reference/make_html_styles.md)
  : vectorized make_styles for html span output
- [`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md)
  : vectorized make_styles for crayon output
- [`smoothScatterJam()`](https://jmw86069.github.io/jamba/reference/smoothScatterJam.md)
  : Smooth scatter plot, Jam style
