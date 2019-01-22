---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->


# jamba

The goal of jamba is to provide useful custom functions for R data
analysis and visualization.

## Package Reference

A full online function reference is available via the pkgdown
documentation:

[Full jamba command reference](https://jmw86069.github.io/jamba)

Functions are categorized, some examples are listed below:

* plot - `plotSmoothScatter()`, `imageByColors()`, `minorLogTicksAxis()`
* color - `color2gradient()`, `makeColorDarker()`
* string - `pasteByRow()`, `pasteByRowOrdered()`
* numeric - `rowGroupMeans()`, `rowRmMadOutliers()`
* list - `cPaste()`, `uniques()`, `sdim()`, `rbindList()`
* sort - `mixedSort()`, `mixedSortDF()` - suite of efficient alphanumeric sorts
* date - `dateToDaysOld()`, `getDate()`
* grep - `vgrep()`, `vigrep()`, `provigrep()`, `grepls()`
* practical - `jargs()`, `printDebug()`, `setPrompt()`, `newestFile()`, `renameColumn()`

## Other related Jam packages

* `colorjam` -- package focused on use of color, with some cross-over with
functions in `jamba`. The `colorjam` package defines a set of categorical
colors or arbitrary length, which are applied in a variety of ways to
experimental designs. The color functions in `jamba` may soon be
transitioned to the `colorjam` package, mainly for consistency.
* `jamma` -- package focused on creating MA-plots, which relies heavily
upon the `plotSmoothScatter()` function in `jamba`.


