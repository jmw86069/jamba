# jamba

The goal of jamba is to provide useful custom functions for R data
analysis and visualization. jamba version **1.0.2**

## Package Reference

A full online function reference is available via the pkgdown
documentation:

[Full jamba command reference](https://jmw86069.github.io/jamba/)

Functions are categorized, some examples are listed below:

## Installation

Production will soon be available from CRAN:

`install.packages("jamba")`

The development version can be installed:

`remotes::install_github("jmw86069/jamba")`

### Additional R Packages in “Suggests”

- `crayon` - install with `install.packages("crayon")` for glorious
  colored console output. Color makes it better.
- `farver` - install with `install.packages("farver")` for more
  efficient color manipulations, and HSL color coneversions.

### Additional R Packages in “Enhances”

Bioconductor packages are invaluable for bioinformatics work, but can be
a bit “heavy” to install if not absolutely necessary. Therefore,
Bioconductor packages are in “Enhances” so they require someone to make
the choice to install them.

- `S4Vectors` - install with `BiocManager::install("S4vectors")` to
  improve speed of
  [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md)
  functions.
- `openxlsx` - install with `install.packages("openxlsx")` to support
  Excel `xlsx` file import, and stylized export.
- `kableExtra` - install with `install.packages("kableExtra")` to enable
  colorized kable HTML tables in RMarkdown documents.
- `ComplexHeatmap` - install with
  `BiocManager::install("ComplexHeatmap")` to use with
  [`heatmap_row_order()`](https://jmw86069.github.io/jamba/reference/heatmap_row_order.md),
  [`cell_fun_label()`](https://jmw86069.github.io/jamba/reference/cell_fun_label.md)
  for custom labels.
- `matrixStats` - install with `install.packages("matrixStats")` for
  efficient `numeric` stats calculations, or `sparseMatrixStats` for use
  with Matrix sparse matrices as used with Seurat and
  SingleCellExperiment data.
- `ggridges` - install with `install.packages("ggridges")` for
  convenient ridge density plots using
  [`plotRidges()`](https://jmw86069.github.io/jamba/reference/plotRidges.md).

## Background

The R functions in `jamba` have been built up, used, tested, revised
over several years. They are immediately useful for day-to-day work, and
efficient and robust enough for production pipelines.

Many were inspired by discussion from Stackoverflow, R-help, or
Bioconductor, with citations thanking principal author(s). Many thanks
to the original authors! The R community is built upon the collective
greatness of its contributors!

Most of the functions are designed around workflows for Bioinformatics
analyses, where functions need to be efficient when operating over
10,000 to 100,000 elements. (They work quite well with millions as
well.) Usually the speed gains are obvious with about 100 elements, then
scale linearly (or worse) as the number increases. I and others use
these functions *all the time*.

One example function
[`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
is a simple wrapper around very useful
[`openxlsx::write.xlsx()`](https://rdrr.io/pkg/openxlsx/man/write.xlsx.html),
which also applies column formatting for column types: P-values, fold
changes, log2 fold changes, numeric, and integer values. Columns use
conditional Excel formatting to apply color-shading to cells for each
type.

Similarly,
[`readOpenxlsx()`](https://jmw86069.github.io/jamba/reference/readOpenxlsx.md)
is a wrapper function to
[`openxlsx::read.xlsx()`](https://rdrr.io/pkg/openxlsx/man/read.xlsx.html)
which reads each worksheet and returns a `list` of `data.frame` objects.
It can detect multi-row column headers, for which it returns combined
column names. It also applies equivalent of `check.names=FALSE` so
column names are returned without change.

Small and large efficiencies are used wherever possible. The
[`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
functions are based upon
[`gtools::mixedsort()`](https://rdrr.io/pkg/gtools/man/mixedsort.html),
with additional optimizations for speed and custom needs. It sorts
chromosome names, gene names, micro-RNA names, etc.

## Alphanumeric sort

- [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md) -
  highly efficient alphanumeric sort, for example chr1, chr2, chr3,
  chr10, etc.
- [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md) -
  as above, applied to columns in a `data.frame` (or `matrix`, `tibble`,
  `DataFrame`, etc.)
- [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md) -
  as above, applied to a list of vectors with no speed loss.

Example:

|     | miRNA   | sort_rank | mixedSort_rank |
|:----|:--------|----------:|---------------:|
| 2   | ABCA2   |         2 |              1 |
| 1   | ABCA12  |         1 |              2 |
| 3   | miR-1   |         3 |              3 |
| 6   | miR-1a  |         6 |              4 |
| 7   | miR-1b  |         7 |              5 |
| 8   | miR-2   |         8 |              6 |
| 4   | miR-12  |         4 |              7 |
| 9   | miR-22  |         9 |              8 |
| 5   | miR-122 |         5 |              9 |

## Base R plotting

These functions help with base R plots, in all those little cases when
the amazing `ggplot2` package is not a smooth fit.

- [`nullPlot()`](https://jmw86069.github.io/jamba/reference/nullPlot.md) -
  convenient “blank” base R plot, optionally displays margins
- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md) -
  smooth scatter
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for point
  density, enhanced over
  [`smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html)
  ![Example of smooth scatterplots using the default R function, and
  enhanced function in the jamba
  package.](reference/figures/README-plotSmoothScatter-1.png)
- [`plotPolygonDensity()`](https://jmw86069.github.io/jamba/reference/plotPolygonDensity.md) -
  fast density/histogram plot for vector or matrix ![Example polygon
  density plot.](reference/figures/README-plotPolygonDensity-1.png)
- [`imageDefault()`](https://jmw86069.github.io/jamba/reference/imageDefault.md) -
  enhanced [`image()`](https://rdrr.io/r/graphics/image.html) that
  enables raster output with consistent pixel aspect ratio.
- [`imageByColors()`](https://jmw86069.github.io/jamba/reference/imageByColors.md) -
  wrapper to [`image()`](https://rdrr.io/r/graphics/image.html) for a
  matrix or data.frame of colors, with optional labels ![Example color
  matrix as plotted using the image by colors
  function()](reference/figures/README-imageByColors-1.png)
- [`minorLogTicksAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md),
  [`logFoldAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md),
  [`pvalueAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md) -
  log axis tick marks and labels, compatible with `offset` for example
  `log(offset + x)`.
- [`sqrtAxis()`](https://jmw86069.github.io/jamba/reference/sqrtAxis.md) -
  draw a square-root transformed axis, with proper labels.
- [`drawLabels()`](https://jmw86069.github.io/jamba/reference/drawLabels.md) -
  draw square colorized text labels
- [`shadowText()`](https://jmw86069.github.io/jamba/reference/shadowText.md) -
  replacement for [`text()`](https://rdrr.io/r/graphics/text.html) that
  draws shadows or outlines. ![Example showing text labels, and shadow
  text labels on a colored
  background.](reference/figures/README-labels-1.png)
- [`groupedAxis()`](https://jmw86069.github.io/jamba/reference/groupedAxis.md) -
  grouped axis labels to show regions/ranges
- [`decideMfrow()`](https://jmw86069.github.io/jamba/reference/decideMfrow.md) -
  determine appropriate value for `par("mfrow")` for multipanel output
  in base R plotting.
- [`getPlotAspect()`](https://jmw86069.github.io/jamba/reference/getPlotAspect.md) -
  determine visible plot aspect ratio.

## Excel export

Every Bioinformatician/statistician needs to write data to Excel, the
[`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
function is consistent and makes it look pretty. You can save numerous
worksheets in a single Excel file, without having to go back and
custom-format everything.

- [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md) -
  flexible Excel exporter, with categorical and conditional colors.
- [`applyXlsxCategoricalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxCategoricalFormat.md) -
  apply categorical colors to Excel
- [`applyXlsxConditionalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxConditionalFormat.md) -
  apply conditional colors to Excel

## Colors

Almost everything uses color somewhere, especially on R console, and in
every R plot.

- [`getColorRamp()`](https://jmw86069.github.io/jamba/reference/getColorRamp.md) -
  retrieve or create color palettes
- [`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md) -
  find contrasting font color for colored background
- [`makeColorDarker()`](https://jmw86069.github.io/jamba/reference/makeColorDarker.md) -
  make a color darker (or lighter, or saturated)
- [`color2gradient()`](https://jmw86069.github.io/jamba/reference/color2gradient.md) -
  split one color to a gradient of `n` colors
- [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md) -
  display a vector or `list` of colors
- [`rainbow2()`](https://jmw86069.github.io/jamba/reference/rainbow2.md) -
  enhances [`rainbow()`](https://rdrr.io/r/grDevices/palettes.html)
  categorical colors for visual contrast.
- [`warpRamp()`](https://jmw86069.github.io/jamba/reference/warpRamp.md) -
  “bend” a color gradient to enhance the visual range
- [`fixYellow()`](https://jmw86069.github.io/jamba/reference/fixYellow.md) -
  opinionated reduction of yellow-green hue
- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md) -
  colorized text output to console or RMarkdown
- [`printDebugHtml()`](https://jmw86069.github.io/jamba/reference/printDebug.md) -
  colorized HTML output in RMarkdown or web pages
- [`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md) -
  colored
  [`kableExtra::kable()`](https://rdrr.io/pkg/knitr/man/kable.html)
  RMarkdown tables, if `kableExtra` package is installed.
- [`col2alpha()`](https://jmw86069.github.io/jamba/reference/col2alpha.md),
  [`alpha2col()`](https://jmw86069.github.io/jamba/reference/alpha2col.md) -
  get or set alpha transparency
- [`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md),
  [`col2hsl()`](https://jmw86069.github.io/jamba/reference/col2hsl.md),
  [`col2hsv()`](https://jmw86069.github.io/jamba/reference/col2hsv.md),
  [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md),
  [`hsl2col()`](https://jmw86069.github.io/jamba/reference/hsl2col.md),
  [`hsv2col()`](https://jmw86069.github.io/jamba/reference/hsv2col.md),
  [`rgb2col()`](https://jmw86069.github.io/jamba/reference/rgb2col.md) -
  consistent color conversions.
- [`color_dither()`](https://jmw86069.github.io/jamba/reference/color_dither.md) -
  split color into two to make color stripes

![Image showing a series of color palettes, adjusting contrast with
lens, and expanding palettes with
color2gradient()](reference/figures/README-colorshow-1.png)

## List Functions

Efficient methods to operate on lists in one call, to avoid looping
through the list either with `for()` loops,
[`lapply()`](https://rdrr.io/r/base/lapply.html) or `map()` functions.
Driven by speed with 10k-100k rows, typical biological datasets.

Compared to convenient alternatives,
[`apply()`](https://rdrr.io/r/base/apply.html) or tidyverse, typically
order of magnitude faster. (Ymmv.) Notable exceptions: `data.table` and
Bioconductor `S4Vectors`. Both are amazing, and are fairly heavy
installations. `S4Vectors` is used when available.

- [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md) -
  `paste(..., collapse)` a list of vectors
- [`cPasteS()`](https://jmw86069.github.io/jamba/reference/cPaste.md) -
  [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md)
  with
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
- [`cPasteU()`](https://jmw86069.github.io/jamba/reference/cPaste.md) -
  [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md)
  with [`unique()`](https://rdrr.io/r/base/unique.html) (actually
  [`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md))
- [`cPasteSU()`](https://jmw86069.github.io/jamba/reference/cPaste.md) -
  [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md)
  with
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  and [`unique()`](https://rdrr.io/r/base/unique.html)
- [`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md) -
  [`unique()`](https://rdrr.io/r/base/unique.html) across a list of
  vectors
- [`sclass()`](https://jmw86069.github.io/jamba/reference/sclass.md) -
  [`class()`](https://rdrr.io/r/base/class.html) a list
- [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) -
  [`dim()`](https://rdrr.io/r/base/dim.html) across a list, or S4
  object, or non-list object
- [`ssdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) -
  [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) across
  a list
- [`sdima()`](https://jmw86069.github.io/jamba/reference/sdim.md) -
  [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) for
  [`attributes()`](https://rdrr.io/r/base/attributes.html)
- [`rbindList()`](https://jmw86069.github.io/jamba/reference/rbindList.md) -
  `do.call(rbind, ...)` to bind rows into a `matrix` or `data.frame`,
  useful together with
  [`strsplit()`](https://rdrr.io/r/base/strsplit.html).
- [`mergeAllXY()`](https://jmw86069.github.io/jamba/reference/mergeAllXY.md) -
  `merge(..., all.x=TRUE, all.y=TRUE)` a list of `data.frame`
- [`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md) -
  remove NULL from a list, with optional replacement
- [`rmNAs()`](https://jmw86069.github.io/jamba/reference/rmNAs.md) -
  [`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md) across
  a list, with option replacement(s)
- [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md) -
  display colors
- [`heads()`](https://jmw86069.github.io/jamba/reference/heads.md) -
  [`head()`](https://rdrr.io/r/utils/head.html) across a list

## Unique names with versions

R object names provide an additional method to confirm data are kept in
the proper order. Duplicated names may be silently ignored, which
motivated the easy approach to “make unique names”.

- [`makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md) -
  make unique names, with flexible rules
- [`nameVector()`](https://jmw86069.github.io/jamba/reference/nameVector.md) -
  add unique names using
  [`makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md)
- [`nameVectorN()`](https://jmw86069.github.io/jamba/reference/nameVectorN.md) -
  make vector of names, named with
  [`makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md).
  Useful inside [`lapply()`](https://rdrr.io/r/base/lapply.html) which
  returns names but only when provided.

## data.frame/matrix/tibble

- [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md) -
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  by columns or rownames
- [`pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md) -
  fast row-paste with delimiters, default skips blanks
- [`pasteByRowOrdered()`](https://jmw86069.github.io/jamba/reference/pasteByRowOrdered.md) -
  nifty alternative that honors factor levels
- [`rowGroupMeans()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md),
  [`rowRmMadOutliers()`](https://jmw86069.github.io/jamba/reference/rowRmMadOutliers.md) -
  grouped row functions
- [`mergeAllXY()`](https://jmw86069.github.io/jamba/reference/mergeAllXY.md) -
  merge a list of `data.frame` into one, keeping all rows
- [`renameColumn()`](https://jmw86069.github.io/jamba/reference/renameColumn.md) -
  rename columns `from` and `to`.
- [`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md) -
  flexible colorized `data.frame` output in Rmarkdown.

## String / grep

- [`tcount()`](https://jmw86069.github.io/jamba/reference/tcount.md) -
  [`table()`](https://rdrr.io/r/base/table.html) sorted high-to-low,
  with minimum count filter
- [`middle()`](https://jmw86069.github.io/jamba/reference/middle.md) -
  show `n` entries from start, middle, then end.
- [`gsubOrdered()`](https://jmw86069.github.io/jamba/reference/gsubOrdered.md) -
  [`gsub()`](https://rdrr.io/r/base/grep.html) that returns ordered
  factor, inherits existing
- [`gsubs()`](https://jmw86069.github.io/jamba/reference/gsubs.md) -
  [`gsub()`](https://rdrr.io/r/base/grep.html) a vector of
  patterns/replacements.
- [`grepls()`](https://jmw86069.github.io/jamba/reference/grepls.md) -
  grep the environment object names, including attached packages
- [`vgrep()`](https://jmw86069.github.io/jamba/reference/vgrep.md),
  [`vigrep()`](https://jmw86069.github.io/jamba/reference/vigrep.md) -
  value-grep shortcut
- `unvgrep()`,
  [`unvigrep()`](https://jmw86069.github.io/jamba/reference/unvigrep.md) -
  un-grep, remove matched results
- [`provigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md) -
  progressive grep, returns matches in order of patterns
- [`igrepHas()`](https://jmw86069.github.io/jamba/reference/igrepHas.md) -
  case-insensitive grep-any
- [`ucfirst()`](https://jmw86069.github.io/jamba/reference/ucfirst.md) -
  upper-case the first letter of each word.
- [`padString()`](https://jmw86069.github.io/jamba/reference/padString.md),
  [`padInteger()`](https://jmw86069.github.io/jamba/reference/padInteger.md) -
  produce strings from numeric values with consistent leading zeros.

## Numeric

- [`formatInt()`](https://jmw86069.github.io/jamba/reference/formatInt.md) -
  opinionated [`format()`](https://rdrr.io/r/base/format.html) for
  integers.
- [`normScale()`](https://jmw86069.github.io/jamba/reference/normScale.md) -
  scale between 0 and 1 or custom range
- [`noiseFloor()`](https://jmw86069.github.io/jamba/reference/noiseFloor.md) -
  apply noise floor, ceiling, with flexible replacements
- [`log2signed()`](https://jmw86069.github.io/jamba/reference/log2signed.md),
  [`exp2signed()`](https://jmw86069.github.io/jamba/reference/exp2signed.md) -
  log2 with offset, and reciprocal
- [`rowGroupMeans()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md),
  [`rowRmMadOutliers()`](https://jmw86069.github.io/jamba/reference/rowRmMadOutliers.md) -
  efficient grouped row functions
- [`deg2rad()`](https://jmw86069.github.io/jamba/reference/deg2rad.md),
  [`rad2deg()`](https://jmw86069.github.io/jamba/reference/rad2deg.md) -
  interconvert degrees and radians
- [`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md) -
  remove NA values, with optional replacement
- [`warpAroundZero()`](https://jmw86069.github.io/jamba/reference/warpAroundZero.md) -
  warp a numeric vector symmetrically around zero
- [`rmInfinite()`](https://jmw86069.github.io/jamba/reference/rmInfinite.md) -
  remove infinite values, with optional replacement.
- [`formatInt()`](https://jmw86069.github.io/jamba/reference/formatInt.md) -
  convenient [`format()`](https://rdrr.io/r/base/format.html) for
  integer output, with comma-delimiter by default

### Common usage

- convert zero to NA:

``` r

noiseFloor(0:10, minimum=1e-20, newValue=NA)
#>  [1] NA  1  2  3  4  5  6  7  8  9 10
```

- convert values below floor to floor:

``` r

noiseFloor(0:10, minimum=3)
#>  [1]  3  3  3  3  4  5  6  7  8  9 10
```

- convert values below floor or NA to floor:

``` r

noiseFloor(c(0:10, NA), minimum=3, adjustNA=TRUE)
#>  [1]  3  3  3  3  4  5  6  7  8  9 10  3
```

## Practical / helpful

- [`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md) -
  pretty function arguments, optional pattern search argument name

``` r

jargs(plotSmoothScatter)
#>                 x = ,
#>                 y = NULL,
#>              bwpi = 50,
#>             binpi = 50,
#>        bandwidthN = NULL,
#>              nbin = NULL,
#>            expand = c(0.04, 0.04),
#>       transFactor = 0.25,
#>    transformation = function( x ) x^transFactor,
#>              xlim = NULL,
#>              ylim = NULL,
#>              xlab = NULL,
#>              ylab = NULL,
#>          nrpoints = 0,
#>           colramp = c("white", "lightblue", "blue", "orange", "orangered2"),
#>               col = "black",
#>            doTest = FALSE,
#>    fillBackground = TRUE,
#>          naAction = c("remove", "floor0", "floor1"),
#>              xaxt = "s",
#>              yaxt = "s",
#>               add = FALSE,
#>               asp = NULL,
#> applyRangeCeiling = TRUE,
#>         useRaster = TRUE,
#>           verbose = FALSE,
#>               ... =
```

- [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md),
  [`ssdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) -
  dimensions of list objects, or nested list of lists
- [`sdima()`](https://jmw86069.github.io/jamba/reference/sdim.md) - runs
  [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) on the
  attributes of an object.
- [`isTRUEV()`](https://jmw86069.github.io/jamba/reference/isTRUEV.md),
  [`isFALSEV()`](https://jmw86069.github.io/jamba/reference/isFALSEV.md) -
  vectorized test for TRUE or FALSE values, since
  [`isTRUE()`](https://rdrr.io/r/base/Logic.html) only operates on
  single values, and does not allow `NA`.
- [`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md) -
  load RMarkdown cache folder into environment
- [`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md) -
  for developers, call child function while passing only acceptable
  arguments in `...`. Instead of: `something(x, ...)`, use:
  `call_fn_ellipsis(something, x, ...)` and never worry about `...`.
- [`log2signed()`](https://jmw86069.github.io/jamba/reference/log2signed.md),
  [`exp2signed()`](https://jmw86069.github.io/jamba/reference/exp2signed.md) -
  convenient `log2(1 + x)` or its reciprocal, using customizable offset.
- [`newestFile()`](https://jmw86069.github.io/jamba/reference/newestFile.md) -
  most recently modified file from a vector of files

### R console

- [`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md) - Jam
  argument list - see “Practical” above for example
- [`lldf()`](https://jmw86069.github.io/jamba/reference/lldf.md) -
  [`ls()`](https://rdrr.io/r/base/ls.html) with
  [`object.size()`](https://rdrr.io/r/utils/object.size.html) into
  `data.frame`
- [`middle()`](https://jmw86069.github.io/jamba/reference/middle.md) -
  Similar to [`head()`](https://rdrr.io/r/utils/head.html) and
  [`tail()`](https://rdrr.io/r/utils/head.html),
  [`middle()`](https://jmw86069.github.io/jamba/reference/middle.md)
  shows `n` entries from beginning, middle, to end.
- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md) -
  colorized text output
- [`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md) -
  colorized R console prompt with project name and R version

### RMarkdown

- [`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md) -
  when rendering RMarkdown with `cache=TRUE`, this function reads the
  cache to reload the environment without re-processing, to recover the
  exact result for continued work.

- [`printDebugHtml()`](https://jmw86069.github.io/jamba/reference/printDebug.md) -
  colored HTML output.

  - Shortcut for `printDebug(..., htmlOut=TRUE, comments=FALSE)`, or
    `options("jam.htmlOut"=TRUE, "jam.comment"=FALSE)`.
  - The RMarkdown chunk must include: `results='asis'`

``` r

printDebugHtml("printDebugHtml(): ",
  "Output is colorized: ",
  head(LETTERS, 8))
```

(12:05:41) 07Mar2025: printDebugHtml(): Output is colorized:
A,B,C,D,E,F,G,H  

``` r


withr::with_options(list(jam.htmlOut=TRUE, jam.comment=FALSE), {
  printDebugHtml(c("printDebug() using withr::with_options(): "),
    c("Output should be colorized: "),
    head(LETTERS, 8));
})
```

(12:05:41) 07Mar2025: printDebug() using withr::with_options(): Output
should be colorized: A,B,C,D,E,F,G,H  

- [`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md) -
  applies categorical colors to `kable()` output using
  [`kableExtra::kable()`](https://rdrr.io/pkg/knitr/man/kable.html).

  - It also applies a contrasting text color.
  - Unfortunately, the HTML output is not compatible with this page on
    Github, see package function docs in RStudio.

``` r

expt_df <- data.frame(
  Sample_ID="",
  Treatment=rep(c("Vehicle", "Dex"), each=6),
  Genotype=rep(c("Wildtype", "Knockout"), each=3),
  Rep=paste0("rep", c(1:3)))
expt_df$Sample_ID <- pasteByRow(expt_df[, 2:4])

# define colors
colorSub <- c(Vehicle="palegoldenrod",
  Dex="navy",
  Wildtype="gold",
  Knockout="firebrick",
  nameVector(color2gradient("grey48", n=3, dex=10), rep("rep", 3), suffix=""),
  nameVector(
    color2gradient(n=3,
      c("goldenrod1", "indianred3", "royalblue3", "darkorchid4")),
    expt_df$Sample_ID))
kbl <- kable_coloring(
  expt_df,
  caption="Experiment design table showing categorical color assignment.",
  colorSub)
```

## Other related Jam packages

Jam Github R packages are being transitioned to CRAN/Bioconductor:

- `venndir`: Venn diagrams with direction, designed for published
  figures.
- `multienrichjam`: Multi-enrichment pathway analysis and visualization
  tools.
- `splicejam`: Sashimi plots for RNA-seq coverage and junction data.
- `jamma`: MA-plots as a unified ***data signal*** quality control
  toolset.
- `colorjam`: `rainbowJam()`, Categorical colors with improved visual
  contrast.
- `genejam`: Fast, structured approach to gene symbol integration.
- `platjam`: Platform specific functions: Nanostring, Salmon,
  Proteomics, Lipidomics; NGS coverage heatmaps.
- `jamses`: `heatmap_se()` friendly wrapper for ComplexHeatmap; other
  integrated methods for factor-aware design/contrasts, normalization,
  contrasts, heatmaps.
- `jamsession`: properly save/load R objects, R sessions, R functions.
