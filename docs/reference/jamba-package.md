# jamba: Jam Base Methods

The jamba package contains several jam base functions which are
re-usable for routine R analysis work, and are important dependencies
for other Jam R packages.

## Details

See the function reference for a complete list of functions.

The goal is to implement methods as lightweight as possible, so so
inclusion in an analysis workflow will not incur a noticeable burden.

## plot functions

- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  enhanced variant of
  [`smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html) to
  show more visual detail

- [`imageDefault()`](https://jmw86069.github.io/jamba/reference/imageDefault.md)
  enhanced rasterized [`image()`](https://rdrr.io/r/graphics/image.html)
  with fixed aspect ratio

- [`imageByColors()`](https://jmw86069.github.io/jamba/reference/imageByColors.md)
  for `data.frame` of colors and optional labels centered across
  repeated values.

- [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md)
  color display for vector, list, color function, or mixed formats.

- [`nullPlot()`](https://jmw86069.github.io/jamba/reference/nullPlot.md)
  blank plot that labels the current margin sizes

- [`minorLogTicksAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md)
  log-scale axis ticks in base R with custom log base, optional offset,
  e.g. `log2(1 + x)`

- [`shadowText()`](https://jmw86069.github.io/jamba/reference/shadowText.md)
  base R text labels with shadow or outline or both, also
  [`shadowText_options()`](https://jmw86069.github.io/jamba/reference/shadowText_options.md).

- [`getPlotAspect()`](https://jmw86069.github.io/jamba/reference/getPlotAspect.md),
  [`decideMfrow()`](https://jmw86069.github.io/jamba/reference/decideMfrow.md)
  convenience base R graphics.

## string functions

- [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md),
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md),
  [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md) -
  efficient alphanumeric "version" sort, with options helpful for gene
  symbols.

- [`vgrep()`](https://jmw86069.github.io/jamba/reference/vgrep.md),
  [`vigrep()`](https://jmw86069.github.io/jamba/reference/vigrep.md),
  [`igrep()`](https://jmw86069.github.io/jamba/reference/igrep.md),
  [`vigrep()`](https://jmw86069.github.io/jamba/reference/vigrep.md)
  fast grep wrappers for value-return, case-insensitive search.

- [`provigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md),
  [`proigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md) -
  progressive, ordered grep to use pattern matching to re-order a
  vector.

- [`makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md)
  create unique, versioned names with custom format

- [`nameVector()`](https://jmw86069.github.io/jamba/reference/nameVector.md)
  apply names to vector dynamically

- [`nameVectorN()`](https://jmw86069.github.io/jamba/reference/nameVectorN.md)
  vector of named names useful with
  [`lapply()`](https://rdrr.io/r/base/lapply.html).

- [`pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md),
  [`pasteByRowOrdered()`](https://jmw86069.github.io/jamba/reference/pasteByRowOrdered.md)
  paste data.frame and matrix values by row, skipping blanks, optional
  factor order.

- [`rbindList()`](https://jmw86069.github.io/jamba/reference/rbindList.md)
  convert list to `matrix` or `data.frame`.

- [`tcount()`](https://jmw86069.github.io/jamba/reference/tcount.md)
  extends [`table()`](https://rdrr.io/r/base/table.html) to sort by size
  and optional minimum count filter.

## color functions

- [`rgb2col()`](https://jmw86069.github.io/jamba/reference/rgb2col.md),
  [`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md),
  [`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md),
  [`col2hsv()`](https://jmw86069.github.io/jamba/reference/col2hsv.md),
  [`hsv2col()`](https://jmw86069.github.io/jamba/reference/hsv2col.md)
  color interconversion

- [`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md)
  text contrast color per given background color

- [`getColorRamp()`](https://jmw86069.github.io/jamba/reference/getColorRamp.md)
  catch-all to get named gradients, or expand one or more colors to
  gradient.

- [`makeColorDarker()`](https://jmw86069.github.io/jamba/reference/makeColorDarker.md),
  [`color2gradient()`](https://jmw86069.github.io/jamba/reference/color2gradient.md)
  create light/dark altered variants of colors.

- [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md)
  versatile display of color as `character` vector or `list` of
  `character` or `function`.

## miscellaneous helper functions

- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  colored text output to console, 'Rmarkdown', HTML

- [`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md)
  colored
  [`kableExtra::kable()`](https://rdrr.io/pkg/knitr/man/kable.html)
  output for 'Rmarkdown'

- [`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)
  colored R prompt

- [`deg2rad()`](https://jmw86069.github.io/jamba/reference/deg2rad.md),
  [`rad2deg()`](https://jmw86069.github.io/jamba/reference/rad2deg.md)
  interconvert degrees to radians.

- [`getDate()`](https://jmw86069.github.io/jamba/reference/getDate.md),
  [`asDate()`](https://jmw86069.github.io/jamba/reference/asDate.md),
  [`dateToDaysOld()`](https://jmw86069.github.io/jamba/reference/dateToDaysOld.md)
  human-readable, opinionated date formatting

- [`padString()`](https://jmw86069.github.io/jamba/reference/padString.md),
  [`padInteger()`](https://jmw86069.github.io/jamba/reference/padInteger.md)
  pad character or integer strings

- [`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md),
  [`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md),
  [`rmInfinite()`](https://jmw86069.github.io/jamba/reference/rmInfinite.md)
  remove or replace missing or NA values with defined alternatives

## export and import functions

- [`readOpenxlsx()`](https://jmw86069.github.io/jamba/reference/readOpenxlsx.md)
  import worksheets from 'xlsx' 'Excel' files.

- [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
  export worksheets to 'xlsx' 'Excel' files with color, formatting, and
  styling.

- [`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md)
  load Rmarkdown cache data into an `environment` for re-use.

- [`reload_qmd_cache()`](https://jmw86069.github.io/jamba/reference/reload_qmd_cache.md)
  load Quarto .qmd cache data into an `environment` for re-use.

## Jam options

The `jamba` package recognizes some global options, but limits these
options to include only non-analysis options. For example, no global
option should change the numerical manipulation of data.

- `jam.lightMode` - `logical` whether the R console or graphical
  background is light or dark,
  [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  limits the luminance range to maximize visual contrast.

- `jam.Crange`,`jam.Lrange` - numerical values used by
  [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  to maximize visual contrast, used with `jam.lightMode`.

- `jam.shadowColor`,`jam.shadow.r`,`jam.shadow.n`,`jam.alphaShadow`,
  `jam.outline`,`jam.alphaOutline` to customize details for
  [`shadowText()`](https://jmw86069.github.io/jamba/reference/shadowText.md),
  see
  [`shadowText_options()`](https://jmw86069.github.io/jamba/reference/shadowText_options.md)
  for convenience.

## See also

Useful links:

- <https://jmw86069.github.io/jamba/>

- Report bugs at <https://github.com/jmw86069/jamba/issues>

## Author

**Maintainer**: James M. Ward <jmw86069@gmail.com>
([ORCID](https://orcid.org/0000-0002-9510-2848)) \[copyright holder\]

Authors:

- James M. Ward <jmw86069@gmail.com>
  ([ORCID](https://orcid.org/0000-0002-9510-2848)) \[copyright holder\]
