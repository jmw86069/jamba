
<!-- README.md is generated from README.Rmd. Please edit that file -->
jamba
=====

The goal of jamba is to provide useful custom functions for R data analysis and visualization.

Package Reference
-----------------

A full online function reference is available via the pkgdown documentation:

[Full jamba command reference](https://jmw86069.github.io/jamba)

Functions are categorized, some examples are listed below:

Background
----------

The R functions in `jamba` have been built up over several years, often based upon and citing the relevant discussion from Stackoverflow, R-help, or Bioconductor, along with the principal author(s). Almost every function is some sort of wrapper around existing R functions -- designed for specific cases where I can make it faster, more flexible, or customized to make my analysis life easier. Kudos and thanks to the original authors! The R community is built upon the collective greatness of its contributors!

Most of the functions are designed around workflows for Bioinformatics analyses, where functions need to be efficient when operating over 10,000 to 100,000 elements. (They work really well with millions as well.) Usually the speed gains are obvious with about 100 elements, then scale linearly (or worse) as the number increases. I and others use these functions *all the time*.

Small and large efficiencies are used wherever possible. For example it is faster to operate on unique entries from a 100,000 element list, than it is to perform a function on the full list. In most cases, I have tested numerous available R methods and packages, and settled on the fastest\* available at the time. If there is something faster or better, I would *love* for you to let me/us know!

The functions in `jamba` are intended to be convenient wrappers around whatever series of steps it takes to get the job done. My design goal is to "make my own analysis jobs easier" as first priority.

-   If I don't find it useful, nobody else will.
-   And even if nobody else finds it useful, at least I do!

Lastly, `jamba` should motivate me and others to create R packages instead of a random collection of R functions in `*.R` files.

Example R functions
-------------------

### Efficient alphanumeric sort

-   `mixedSort()` - highly efficient alphanumeric sort, for example chr1, chr2, chr3, chr10, etc.
-   `mixedSortDF()` - as above, applied to columns in a `data.frame` (or `matrix`, `tibble`, `DataFrame`, etc.)
-   `mixedSorts()` - as above, applied to a list of vectors with no speed loss.

### Base R plotting

These functions help with base R plots, in all those little cases when the amazing `ggplot2` package is not a smooth fit.

-   `nullPlot()` - convenient "blank" base R plot, optionally displays margins
-   `plotSmoothScatter()` - smooth scatter `plot()` for point density, enhanced over `smoothScatter()`
-   `plotPolygonDensity()` - fast density/histogram plot for vector or matrix
-   `imageDefault()` - enhanced `image()` that enables raster output with consistent pixel aspect ratio.
-   `imageByColors()` - wrapper to `image()` for a matrix or data.frame of colors, with optional labels
-   `minorLogTicksAxis()` - log-transformed axis labels, flexible log base, and option for properly adjusted `log2(1 + x)` format.
-   `sqrtAxis()` - draw a square-root transformed axis, with proper labels.
-   `drawLabels()` - draw square colorized text labels
-   `shadowText()` - replacement for `text()` that draws shadows or outlines.
-   `groupedAxis()` - grouped axis labels to show regions/ranges
-   `decideMfrow()` - determine appropriate value for `par("mfrow")` for multipanel output in base R plotting.
-   `getPlotAspect()` - determine visible plot aspect ratio.

### Excel export

Every Bioinformatician/statistician needs to write data to Excel, the `writeOpenxlsx()` function is consistent and makes it look pretty. You can save numerous worksheets in a single Excel file, without having to go back and custom-format everything.

-   `writeOpenxlsx()` - flexible Excel exporter, with categorical and conditional colors.
-   `applyXlsxCategorical()` - apply categorical colors to Excel
-   `applyXlsxConditional()` - apply conditional colors to Excel

### Color

Everything I do uses color to the utmost limit, especially on R console, and in every R plot.

-   `getColorRamp()` - flexible to create or retrieve color gradients
-   `warpRamp()` - "bend" a color gradient to enhance the visual range
-   `color2gradient()` - convert a color to gradient of n colors; or do the same for a vector
-   `makeColorDarker()` - adjust darkness and saturation
-   `showColors()` - display a vector or list of colors
-   `fixYellow()` - adjust the weird green-yellow, by personal preference
-   `printDebug()` - pretty colorized text output using `crayon` package.

### List

Cool methods to operate on super-long lists in one call, to avoid looping through the list either with `for()` loops, `lapply()` or `map()` functions.

-   `cPaste()` - highly efficient `paste()` over a large list of vectors
-   `cPasteS()` - as above but using `mixedSort()` before `paste()`.
-   `cPasteU()` - as above but using `uniques()` before `paste()`.
-   `cPasteSU()` - as above but using `mixedSort()` and `uniques()` before `paste()`.
-   `uniques()` - efficient `unique()` over a list of vectors
-   `sclass()` - runs `class()` on a list
-   `sdim()`, `ssdim()` - dimensions of list objects, or nested list of lists
-   `rbindList()` - efficient `do.call(rbind, ...)` to bind rows into a matrix or data.frame, useful when following `strsplit()`.
-   `mergeAllXY()` - merge a list of `data.frame` objects
-   `rmNULL()` - remove NULL or empty elements from a list, with optional replacement

### Names

We use R names as an additional method to make sure everything is kept in the proper order. Many R functions return results using input names, so it helps to have a really solid naming strategy. For the R functions that remove names -- I highly recommend adding them back yourself!

-   `makeNames()` - make unique names, using flexible logic
-   `nameVector()` - add names to a vector, using its own value, or supplied names
-   `nameVectorN()` - make named vector using the names of a vector (useful inside `lapply()`) or any function that returns data using names of the input vector.

### data.frame/matrix/tibble

-   `pasteByRow()` - fast, flexible row-paste with delimiters, optionally remove blanks
-   `pasteByRowOrdered()` - as above but returns ordered factor, using existing factor orders from each column when present
-   `rowGroupMeans()`, `rowRmMadOutliers()` - efficient grouped row functions
-   `mergeAllXY()` - merge a list of `data.frame` into one
-   `renameColumn()` - rename columns `from` and `to`.
-   `kable_coloring()` - flexible colorized `data.frame` output in Rmarkdown.

### String / grep

-   `gsubOrdered()` - gsub that returns ordered factor, maintians the previous factor order
-   `grepls()` - grep the environment (including attached packages) for object names
-   `vgrep()`, `vigrep()` - value-grep shortcut
-   `unvgrep()`, `unvigrep()` - un-grep -- remove matched results from the output.
-   `provigrep()` - progressive grep, searches each pattern in order, returning results in that order
-   `igrepHas()` - rapid case-insensitive grep presence/absense test
-   `ucfirst()` - upper-case the first letter of each word.
-   `padString()`, `padInteger()` - produce strings from numeric values with consistent leading zeros.

### Numeric

-   `noiseFloor()` - apply noise floor (and ceiling) with flexible replacement values
-   `warpAroundZero()` - warp a numeric vector symmetrically around zero
-   `rowGroupMeans()`, `rowRmMadOutliers()` - efficient grouped row functions
-   `deg2rad()`, `rad2deg()` - convert degrees to radians
-   `rmNA()` - remove NA values, with optional replacement
-   `rmInfinite()` - remove infinite values, with optional replacement.
-   `formatInt()` - convenient `format()` for integer output, with comma-delimiter by default

### Practical / helpful

-   `jargs()` - pretty function arguments, optional pattern search argument name
-   `sdim()`, `ssdim()` - dimensions of list objects, or nested list of lists
-   `sdima()` - runs `sdim()` on the attributes of an object.
-   `isTRUEV()`, `isFALSEV()` - vectorized test for TRUE or FALSE values, since `isTRUE()` only operates on single values, and does not allow `NA`.

### R console

-   `printDebug()` - pretty colorized text output using `crayon` package.
-   `setPrompt()` - pretty colorized R console prompt with project name and R version
-   `newestFile()` - most recently modified file from a vector of files

Other related Jam packages
--------------------------

-   `jamma` -- MA-plots (also known as "mean-variance", "Bland-Altman", or "mean-difference" plots), relies upon `jamba::plotSmoothScatter()`; `centerGeneData()` to apply flexible row-centering with optional groups and control samples; `jammanorm()` - normalize data based upon MA-plot output
-   `colorjam` -- `colorjam::rainbowJam()` for scalable categorical colors using alternating luminance and chroma values.
-   `genejam` -- fast, consistent conversion of gene symbols to the most current gene nomenclature
-   `splicejam` -- Sashimi plots for RNA-seq data
-   `multienrichjam` -- multiple gene set enrichment analysis and visualization
-   `platjam` -- platform technology functions, importers for NanoString
