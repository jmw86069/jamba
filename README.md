
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jamba

The goal of jamba is to provide useful custom functions for R data
analysis and visualization.

## Package Reference

A full online function reference is available via the pkgdown
documentation:

[Full jamba command reference](https://jmw86069.github.io/jamba/)

Functions are categorized, some examples are listed below:

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

One example function `writeOpenxlsx()` is a simple wrapper around very
useful `openxlsx::write.xlsx()`, which also applies column formatting
for column types: P-values, fold changes, log2 fold changes, numeric,
and integer values. Columns use conditional Excel formatting to apply
color-shading to cells for each type.

Similarly, `readOpenxlsx()` is a wrapper function to
`openxlsx::read.xlsx()` which reads each worksheet and returns a `list`
of `data.frame` objects. It can detect multi-row column headers, for
which it returns combined column names. It also applies equivalent of
`check.names=FALSE` so column names are returned without change.

Small and large efficiencies are used wherever possible. The
`mixedSort()` functions are based upon `gtools::mixedsort()`, with
additional optimizations for speed and custom needs. It sorts chromosome
names, gene names, micro-RNA names, etc.

## Example R functions

### Efficient alphanumeric sort

- `mixedSort()` - highly efficient alphanumeric sort, for example chr1,
  chr2, chr3, chr10, etc.
- `mixedSortDF()` - as above, applied to columns in a `data.frame` (or
  `matrix`, `tibble`, `DataFrame`, etc.)
- `mixedSorts()` - as above, applied to a list of vectors with no speed
  loss.

Example:

<div class="kable-table">

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

</div>

### Base R plotting

These functions help with base R plots, in all those little cases when
the amazing `ggplot2` package is not a smooth fit.

- `nullPlot()` - convenient “blank” base R plot, optionally displays
  margins
- `plotSmoothScatter()` - smooth scatter `plot()` for point density,
  enhanced over `smoothScatter()`
  <img src="man/figures/README-plotSmoothScatter-1.png" alt="Example of smooth scatterplots using the default R function, and enhanced function in the jamba package."  />
- `plotPolygonDensity()` - fast density/histogram plot for vector or
  matrix
  <img src="man/figures/README-plotPolygonDensity-1.png" alt="Example polygon density plot."  />
- `imageDefault()` - enhanced `image()` that enables raster output with
  consistent pixel aspect ratio.
- `imageByColors()` - wrapper to `image()` for a matrix or data.frame of
  colors, with optional labels
  <img src="man/figures/README-imageByColors-1.png" alt="Example color matrix as plotted using the image by colors function()"  />
- `minorLogTicksAxis()` - log-transformed axis labels, flexible log
  base, and option for properly adjusted `log2(1 + x)` format.
- `sqrtAxis()` - draw a square-root transformed axis, with proper
  labels.
- `drawLabels()` - draw square colorized text labels
- `shadowText()` - replacement for `text()` that draws shadows or
  outlines.
  <img src="man/figures/README-labels-1.png" alt="Example showing text labels, and shadow text labels on a colored background."  />
- `groupedAxis()` - grouped axis labels to show regions/ranges
- `decideMfrow()` - determine appropriate value for `par("mfrow")` for
  multipanel output in base R plotting.
- `getPlotAspect()` - determine visible plot aspect ratio.

### Excel export

Every Bioinformatician/statistician needs to write data to Excel, the
`writeOpenxlsx()` function is consistent and makes it look pretty. You
can save numerous worksheets in a single Excel file, without having to
go back and custom-format everything.

- `writeOpenxlsx()` - flexible Excel exporter, with categorical and
  conditional colors.
- `applyXlsxCategoricalFormat()` - apply categorical colors to Excel
- `applyXlsxConditionalFormat()` - apply conditional colors to Excel

### Color

Almost everything uses color somewhere, especially on R console, and in
every R plot.

- `getColorRamp()` - flexible to create or retrieve color gradients
- `warpRamp()` - “bend” a color gradient to enhance the visual range
- `color2gradient()` - convert a color to gradient of n colors; or do
  the same for a vector
- `makeColorDarker()` - adjust darkness and saturation
- `showColors()` - display a vector or list of colors
- `fixYellow()` - adjust the weird green-yellow, by personal preference
- `printDebug()` - pretty colorized text output using `crayon` package.
- `rainbow2()` - rainbow categorical colors with enhanced visual
  contrast.

<img src="man/figures/README-colorshow-1.png" alt="Image showing a series of color palettes, adjusting contrast with lens, and expanding palettes with color2gradient()"  />

### List

Cool methods to operate on super-long lists in one call, to avoid
looping through the list either with `for()` loops, `lapply()` or
`map()` functions.

- `cPaste()` - highly efficient `paste()` over a large list of vectors
- `cPasteS()` - as above but using `mixedSort()` before `paste()`.
- `cPasteU()` - as above but using `uniques()` before `paste()`.
- `cPasteSU()` - as above but using `mixedSort()` and `uniques()` before
  `paste()`.
- `uniques()` - efficient `unique()` over a list of vectors
- `sclass()` - runs `class()` on a list
- `sdim()`, `ssdim()` - dimensions of list objects, or nested list of
  lists
- `rbindList()` - efficient `do.call(rbind, ...)` to bind rows into a
  matrix or data.frame, useful when following `strsplit()`.
- `mergeAllXY()` - merge a list of `data.frame` objects
- `rmNULL()` - remove NULL or empty elements from a list, with optional
  replacement

### Names

We use R names as an additional method to make sure everything is kept
in the proper order. Many R functions return results using input names,
so it helps to have a really solid naming strategy. For the R functions
that remove names – I highly recommend adding them back yourself!

- `makeNames()` - make unique names, using flexible logic
- `nameVector()` - add names to a vector, using its own value, or
  supplied names
- `nameVectorN()` - make named vector using the names of a vector
  (useful inside `lapply()`) or any function that returns data using
  names of the input vector.

### data.frame/matrix/tibble

- `pasteByRow()` - fast, flexible row-paste with delimiters, optionally
  remove blanks
- `pasteByRowOrdered()` - as above but returns ordered factor, using
  existing factor orders from each column when present
- `rowGroupMeans()`, `rowRmMadOutliers()` - efficient grouped row
  functions
- `mergeAllXY()` - merge a list of `data.frame` into one
- `renameColumn()` - rename columns `from` and `to`.
- `kable_coloring()` - flexible colorized `data.frame` output in
  Rmarkdown.

### String / grep

- `gsubOrdered()` - gsub that returns ordered factor, maintians the
  previous factor order
- `grepls()` - grep the environment (including attached packages) for
  object names
- `vgrep()`, `vigrep()` - value-grep shortcut
- `unvgrep()`, `unvigrep()` - un-grep – remove matched results from the
  output.
- `provigrep()` - progressive grep, searches each pattern in order,
  returning results in that order
- `igrepHas()` - rapid case-insensitive grep presence/absense test
- `ucfirst()` - upper-case the first letter of each word.
- `padString()`, `padInteger()` - produce strings from numeric values
  with consistent leading zeros.

### Numeric

- `noiseFloor()` - apply noise floor (and ceiling) with flexible
  replacement values
- `warpAroundZero()` - warp a numeric vector symmetrically around zero
- `rowGroupMeans()`, `rowRmMadOutliers()` - efficient grouped row
  functions
- `deg2rad()`, `rad2deg()` - convert degrees to radians
- `rmNA()` - remove NA values, with optional replacement
- `rmInfinite()` - remove infinite values, with optional replacement.
- `formatInt()` - convenient `format()` for integer output, with
  comma-delimiter by default

### Practical / helpful

- `jargs()` - pretty function arguments, optional pattern search
  argument name

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

- `sdim()`, `ssdim()` - dimensions of list objects, or nested list of
  lists
- `sdima()` - runs `sdim()` on the attributes of an object.
- `isTRUEV()`, `isFALSEV()` - vectorized test for TRUE or FALSE values,
  since `isTRUE()` only operates on single values, and does not allow
  `NA`.

### R console

- `printDebug()` - pretty colorized text output using `crayon` package.
- `setPrompt()` - pretty colorized R console prompt with project name
  and R version
- `newestFile()` - most recently modified file from a vector of files

## Other related Jam packages

- `jamma` – MA-plots (also known as “mean-variance”, “Bland-Altman”, or
  “mean-difference” plots), relies upon `jamba::plotSmoothScatter()`;
  `centerGeneData()` to apply flexible row-centering with optional
  groups and control samples; `jammanorm()` - normalize data based upon
  MA-plot output
- `colorjam` – `colorjam::rainbowJam()` for scalable categorical colors
  using alternating luminance and chroma values.
- `genejam` – fast, consistent conversion of gene symbols to the most
  current gene nomenclature
- `splicejam` – Sashimi plots for RNA-seq data
- `multienrichjam` – multiple gene set enrichment analysis and
  visualization
- `platjam` – platform technology functions, importers for NanoString
