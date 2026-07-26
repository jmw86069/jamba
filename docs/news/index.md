# Changelog

## jamba 1.0.5

- Changed various `NULL` to ‘NULL’ in help docs.

### Test Error Fix

- ‘test-kable-coloring.R’ was updated to compare the computed RGB color
  to the
  [`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md)
  output, targeting the test error seen only with M1mac.

### New functions

- [`reload_qmd_cache()`](https://jmw86069.github.io/jamba/reference/reload_qmd_cache.md)
  which mimics
  [`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md)
  to reload Quarto ‘.qmd’ cache data. Currently requires cache path, and
  path to the Quarto ‘.qmd’ file. See help docs for caveats.

### Enhancements to Existing Functions

- [`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md)
  now removes any duplicated argument names in the input ‘…’ before
  passing to the target function. Added tests.
- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  now uses [`message()`](https://rdrr.io/r/base/message.html) instead of
  [`cat()`](https://rdrr.io/r/base/cat.html) so it can be silenced. This
  was a previous CRAN suggestion, appreciated. Arguments ‘htmlOutput’
  and ‘comment’ detect knitr running, and knitr HTML output, and then
  defines suitable defaults. Argument ‘comment’ now uses leading space ’
  \## ’ by default to avoid becoming markdown header.
- [`makeColorDarker()`](https://jmw86069.github.io/jamba/reference/makeColorDarker.md)
  now returns NULL for empty input.
- [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) now
  handles S7 objects, showing dimensions of properties.
- [`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md)
  now recognizes running inside Positron, defaults to TRUE. Positron
  does not provide the theme via an API, and can be set via
  `options('jam.lightMode'=FALSE)`.
- [`getColorRamp()`](https://jmw86069.github.io/jamba/reference/getColorRamp.md)
  returns attribute ‘divergent’ with `logical` when using divergent
  color scale with a vector of colors.
- [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md)
  now accepts ‘ggplot’ and ‘ggproto’ objects, will recognize when
  mapping is defined for ‘colour’ and ‘fill’, and for continuous and
  discrete colors. With colorRamp2 function input, it uses attributes
  ‘legend_at’ and ‘legend_labels’ when defined, instead of ‘breaks’ used
  for the ramp.
- [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md)
  changed so lightMode=NULL will use Crange,Lrange when provided,
  otherwise detects defaults. It does not set options by default.
- [`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md) now
  properly uses lightMode to set text brightness.
- [`minorLogTicksAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md)
  major and minor label adjustment ‘padj’ is applied with mirror
  adjustment negative axis coordinates.
- [`logFoldAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md)
  and
  [`minorLogTicksAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md)
  new default ‘doMinorLabels=NULL’ will set TRUE when there are fewer
  than 18 minor labels in ‘lims’ range, showing minor ticks otherwise
  (when enabled).
- [`minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md)
  now filters output for the ‘lims’ range rounded up to the next highest
  magnitude integer.

## jamba 1.0.4

CRAN release: 2025-03-23

### Changes per CRAN additional issues

- Updated tests in `test-getColorRamp.r` due to obscure,
  non-reproducible error during automated CRAN package checks on MacOS
  aarch64-apple-darwin24.3.0, R-4.5.0 development.

  - The test failed when
    [`getColorRamp()`](https://jmw86069.github.io/jamba/reference/getColorRamp.md)
    returning “#3333ECFF” instead of “#3434ECFF” on R-4.5.0 devel
    version, ‘test-getColorRamp.r:25:4’.
  - The ugly workaround ignores the possibly underlying cause, which
    resembles a rounding error with hex color values.
  - The error was not reproducible on R-4.5.0 platform
    aarch64-apple-darwin20.

## jamba 1.0.3

### Changes per CRAN additional issues

- ‘jamba-vignette.Rmd’ now uses
  [`requireNamespace("kableExtra", quietly=TRUE)`](https://rdrr.io/r/base/ns-load.html).
- [`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md)
  changed [@examplesId](https://github.com/examplesId) to use
  ‘requireNamespace(“kableExtra”, quietly=TRUE)’. Apparently one
  platform (r-oldrel-macos-arm64) did not enforce the check for
  ‘kableExtra’ possibly due to
  [`check_pkg_installed()`](https://jmw86069.github.io/jamba/reference/check_pkg_installed.md)
  differences on that platform. See changes below to
  [`check_pkg_installed()`](https://jmw86069.github.io/jamba/reference/check_pkg_installed.md).

### Changes to existing functions

- [`check_pkg_installed()`](https://jmw86069.github.io/jamba/reference/check_pkg_installed.md)
  was modified to provide two methods:

  - “packagedir” confirms the package directory exists and contains the
    file ‘DESCRIPTION’. It is possible that that check that the file
    exists is enough improvement over simply using
    [`nchar()`](https://rdrr.io/r/base/nchar.html) on the directory,
    that the checks on ‘r-oldrel-macos-arm64’ would now succeed, however
    that environment is not available for testing. (The development
    environment is already R-4.3.3 on MacOS with M1 chip, though other
    libraries could differ with the official test system.)
  - “requireNamespace” runs
    [`requireNamespace(x, quietly=TRUE)`](https://rdrr.io/r/base/ns-load.html)
    which loads the package namespace but does not attach it. This
    approach is slower, but more accurate.

## jamba 1.0.2

CRAN release: 2025-03-10

### Changes per CRAN comments.

- Used tick marks around software names.
- [`lldf()`](https://jmw86069.github.io/jamba/reference/lldf.md) now
  defaults to `envir=-1L`, the parent environment which is also usually
  [`globalenv()`](https://rdrr.io/r/base/environment.html). It now also
  recognizes `character` and `integer` input, equivalent to `pos` and
  `name` arguments of [`ls()`](https://rdrr.io/r/base/ls.html).
- [`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)
  new default `projectName="unnamed"` matches previous behavior, and no
  longer searches
  [`globalenv()`](https://rdrr.io/r/base/environment.html) for
  `projectName`.

### Potentially breaking changes

- [`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md)

  - Now defaults to `envir=new.env()` instead of using `.GlobalEnv`,
    which was already preferred. It is now the default behavior per CRAN
    policy, and it makes sense.
  - It now returns `envir` invisibly, not the `character` object list.
  - For previous behavior, one can set `envir=.GlobalEnv` or
    `envir=globalenv()` and ignore the output.

## jamba 1.0.1

### Changes per CRAN comments.

- All uses of [`par()`](https://rdrr.io/r/graphics/par.html) and
  [`options()`](https://rdrr.io/r/base/options.html), which previously
  altered its value, were replaced with the `withr` equivalent, to
  preserve the user environment. Notable exception is
  [`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)
  whose change to `options("prompt")` is the specific purpose of the
  function. Other exceptions are Jam options, whose changes are
  intended.

- Removed all cases of `\dontrun{}`, involving the xlsx functions. They
  now use [`tempfile()`](https://rdrr.io/r/base/tempfile.html) and
  perform a proper, small example.

- Confirmed all functions default `verbose=FALSE` except
  [`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md)
  whose verbose output is intended, but the option to silence does
  exist, as it should.

- Removed two stray instances of unintended always-verbose output that
  called
  [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md).
  All calls to [`print()`](https://rdrr.io/r/base/print.html) and
  [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  are wrapped inside `if(verbose){}` blocks.

- [`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md) now
  uses [`message()`](https://rdrr.io/r/base/message.html) instead of
  [`cat()`](https://rdrr.io/r/base/cat.html), so it can be silenced.

- The only remaining use of [`cat()`](https://rdrr.io/r/base/cat.html)
  outside `if(verbose){}` involves
  [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md),
  whose core purpose is to print its output. Upon testing, `cli`
  functions could not be made reliable for HTML/RMarkdown output, even
  using
  [`cli::ansi_html()`](https://cli.r-lib.org/reference/ansi_html.html).

### Other notable changes

- [`decideMfrow()`](https://jmw86069.github.io/jamba/reference/decideMfrow.md)

  - new argument `xyratio=1` to control the target plot aspect ratio.
    Also corrected issue causing blank
  - new argument `trimExtra=TRUE` to control whether blank columns or
    rows are trimmed, to prevent `n=4` from returning `c(3, 2)` in some
    circumstances.

- [`sqrtAxis()`](https://jmw86069.github.io/jamba/reference/sqrtAxis.md)

  - Now passes u5.bias to internal call to pretty(), with `u5.bias=1`
    the new default to maintain previous behavior.

- [`adjustAxisLabelMargins()`](https://jmw86069.github.io/jamba/reference/adjustAxisLabelMargins.md)

  - breaking change: returns `list` named `"mai"` suitable to use with
    `graphics::par(adjustAxisLabelMargins())`.
  - It no longer updates the user
    [`par()`](https://rdrr.io/r/graphics/par.html), due to helpful CRAN
    policy. Makes sense to push this decision outside the function.

- [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md)
  no longer calls
  [`adjustAxisLabelMargins()`](https://jmw86069.github.io/jamba/reference/adjustAxisLabelMargins.md)
  when axis is hidden by `xaxt="n"` or `yaxt="n"` for respective axis
  labels.

### new functions

- [`logFoldAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md),
  [`pvalueAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md)
  easy presets for
  [`minorLogTicksAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md)

## jamba 1.0.0

- CRAN release version.
- Added missing returns, examples.
- Removed global variables.
- Moved farver to Suggests.
- Moved IRanges to Enhances.
- Updated
  [`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md)
  when S4Vectors is not available.
- Added CITATION, updated LICENSE, fixed URL
- Updated some tests to use testthat stop_if logic.

## jamba 0.0.107.900

### changes

- Adjusted function families to improve organization.
- Updated pkgdown site docs.
- Silenced warning in
  [`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md)
- Added alt-text to vignette image output.

### breaking changes

- [`drawLabels()`](https://jmw86069.github.io/jamba/reference/drawLabels.md)

  - new argument `text_fn` used instead of relying upon
    [`text()`](https://rdrr.io/r/graphics/text.html) from global
    environment. It typically uses
    [`graphics::text()`](https://rdrr.io/r/graphics/text.html) but could
    be customized with `text <- jamba::shadowText` to override default
    behavior. It only affects `multienrichjam`, which will be updated
    accordingly.

- Removed for disuse: `tcount2()`, `fix_matrix_ratio()`

## jamba 0.0.106.900

### changes

- Added methods, withr to Imports. Moved RColorBrewer to Imports.
- Moved many packages to Enhances so they will not be installed unless
  necessary for the user Added circlize to Enhances.
- Trimmed DESCRIPTION to CRAN guidance.
- [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md)
  fixed error when `S4Vector` is not installed.
- [`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md)
  enhanced docs, accepts R object and runs
  [`object.size()`](https://rdrr.io/r/utils/object.size.html). Tests.
- Fixed test for rowGroupMeans() with sparse Matrix.
- Fixed
  [`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md)
  for mixed list input when `useBioc=FALSE`, factor and character
  together converted factor to integer then character.
- removed colorjam from Enhances, sigh, since it is only on Github and
  not cleanly available for CRAN checking. Once colorjam is available,
  it will be supported by jamba again.
- Added
  [`rainbow2()`](https://jmw86069.github.io/jamba/reference/rainbow2.md)
  as a cheap alternative to
  [`colorjam::rainbowJam()`](https://jmw86069.github.io/colorjam/reference/rainbowJam.html),
  and quick drop-in replacement for
  [`grDevices::rainbow()`](https://rdrr.io/r/grDevices/palettes.html).

## jamba 0.0.105.900

### bug fixes

- [`rowGroupMeans()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md)

  - Fixed regression when using custom `rowStatsFunc`, added test cases
    to confirm.
  - Fixed regression when using
    [`base::rowMeans()`](https://rdrr.io/r/base/colSums.html) when
    `matrixStats` is available, since it no longer exists in that
    package.
  - Fixed regression by not passing `...` to `rowStatsFunc`, instead it
    only passes `na.rm`. Added test case to confirm.

## jamba 0.0.104.900

### changes to existing functions

- [`rowGroupMeans()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md)
  and
  [`rowRmMadOutliers()`](https://jmw86069.github.io/jamba/reference/rowRmMadOutliers.md)

  - New argument `includeAttributes=FALSE` turns off default attributes
    with number of replicates, but can be re-enabled when needed.
  - Both functions now also support SparseMatrix objects from Matrix,
    which helps when using `Seurat` and `SingleCellExperiment` objects.
  - Both functions will use `matrixStats`, `sparseMatrixStats`, or
    `base` R summary functions based upon the object type and installed
    packages.
  - Added tests for both function to cover the basics.

- Updated all tests to remove deprecated
  [`testthat::context()`](https://testthat.r-lib.org/reference/context.html).

## jamba 0.0.103.900

### Bug fixes

- [`readOpenxlsx()`](https://jmw86069.github.io/jamba/reference/readOpenxlsx.md)

  - Bug was caused when using `startRow` and `rows` causing the column
    headers to use the `rows[startRow]` which was not as intended. The
    logic is now to use `rows` when present, or `startRow` otherwise,
    never both. The help docs now describe this distinction. The same
    logic is applied to `cols` and `startCol`.

## jamba 0.0.102.900

### Bug fixes

- [`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md)

  - It was not properly enforcing HTML output, causing HTML tags to
    appear as HTML tags, and not colorize the text for example.
  - Added test coverage for this scenario.

- [`plotRidges()`](https://jmw86069.github.io/jamba/reference/plotRidges.md)
  now works properly with single vector, single column, and any input
  without [`rownames()`](https://rdrr.io/r/base/colnames.html) or
  [`colnames()`](https://rdrr.io/r/base/colnames.html).

- [`plotPolygonDensity()`](https://jmw86069.github.io/jamba/reference/plotPolygonDensity.md)
  now properly reverts any changes to
  [`par()`](https://rdrr.io/r/graphics/par.html), such as `"mfrow"` and
  axis settings `"xaxs"` and `"yaxs"`.

## jamba 0.0.101.900

### Bug fixes

- [`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md)

  - Fixed mis-placed parentheses in conditional statement, affecting
    only `factor` columns. Apparently introduced with `0.0.100.900`
    below.

## jamba 0.0.100.900

### Bug fixes

- [`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md),
  [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)

  - fixed error when sorting class `"Date"`, `"POSIXct"`, or `"POSIXt"`.
    Previous these columns either caused an error, or were not sorted.
    Now these columns are handled as `numeric` columns, and are
    converted using
    [`as.numeric()`](https://rdrr.io/r/base/numeric.html). Further, the
    actual error arose from converting a `list` to `data.frame` which
    failed for class `"octmode"`. This step is avoided, since
    [`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md)
    operates the same with `list` input anyway. An example is added to
    [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
    using output from
    [`file.info()`](https://rdrr.io/r/base/file.info.html).

### Changes to existing functions

- [`heatmap_row_order()`](https://jmw86069.github.io/jamba/reference/heatmap_row_order.md),
  [`heatmap_column_order()`](https://jmw86069.github.io/jamba/reference/heatmap_column_order.md)

  - new argument `which_heatmap` which allows for returning the row or
    column order of a specific heatmap, when `HeatmapList` is provided.
    It works with horizontal side-by-side heatmap orientation, and
    vertical top-over-bottom heatmap orientation.

- [`breaksByVector()`](https://jmw86069.github.io/jamba/reference/breaksByVector.md)

  - Fixed order of `class(x)` to be `any(X %in% class(x))`.

- [`rgb2col()`](https://jmw86069.github.io/jamba/reference/rgb2col.md)

  - Minor change when detecting `class(red)` to reverse the order to
    `"RGB" %in% class(red)` in case `red` has multiple classes.

- [`pasteByRowOrdered()`](https://jmw86069.github.io/jamba/reference/pasteByRowOrdered.md)

  - Minor change to allow columns to have multiple classes, so they are
    handled accordingly. Should only affect when `keepOrder=TRUE`, which
    checks for presence of `"character"` columns.

## jamba 0.0.99.900

### Bug fixes

- [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md),
  `applyXlsxCategoricalColor()`

  - Fixed bug which caused categorical colors to be applied out of sync
    with columns in the `data.frame`, when called via
    [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md).
    It was caused by using the option `colRange` to apply the
    categorical colors to a subset of data, providing substantial speed
    but at the expense of columns becoming out of sync for columns which
    were not continuously categorically colored starting at column 1.
    The bug is fixed.

## jamba 0.0.98.900

### updates to existing functions

- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)

  - removed erroneous additional newline `"<br/>"` with `htmlOut=TRUE`
  - new arg default `comment=!htmlOut` so `htmlOut=TRUE` by default
    turns off the comment prefix. Usually HTML output is for RMarkdown,
    where the comment prefix causes the text to become a heading, where
    the RMarkdown chunk option uses `results='asis'`.

### new functions

- [`printDebugHtml()`](https://jmw86069.github.io/jamba/reference/printDebug.md)

  - simple wrapper to
    [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
    intended for HTML output, using options `htmlOut=TRUE` and
    `comment=FALSE` specifically for RMarkdown output using chunk option
    `results='asis'`.

## jamba 0.0.97.900

### updates to existing functions

- More functions were updated for CRAN compliance, removing `:::`,
  including `@examples` and `@returns`.

- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)

  - more arguments use
    [`options()`](https://rdrr.io/r/base/options.html) for potential
    overrides
  - removed errant argument `x` which was ignored anyway

## jamba 0.0.96.900

The main update enables `colorSub` input as `list` for several
functions:
[`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md),
[`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md),
and
[`applyXlsxCategoricalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxCategoricalFormat.md).
This feature enables passing a color function, or color assignments that
are specific to each column in the data. The companion function
[`platjam::design2colors()`](https://rdrr.io/pkg/platjam/man/design2colors.html)
takes a `data.frame` and returns a `list` of colors or color functions,
one per column, suitable for use directly in the functions updated
above. The `design2colors()` function will likely move into `colorjam`.

### changes to existing functions

- [`applyXlsxCategoricalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxCategoricalFormat.md)

  - argument `colorSub` now accepts a color `list` named by colnames,
    with either named `character` vector of colors for each column, or a
    `function` that takes column values and returns `character` colors
    for each value.
  - the method for matching `names(colorSub)` to cell values is slightly
    updated, mainly affecting color assignments for `character` strings
    that contain whitespace and punctuation characters.

- [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)

  - argument `colorSub` now accepts either `character` input as
    previously required, or now accepts `list` input named by
    `colnames(x)` whose list elements are either `character` vector of
    colors, or color `function`.
  - Some internal processing is slightly altered to accomodate the
    change,
    [`applyXlsxCategoricalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxCategoricalFormat.md)
    is called with one extra row to include the colnames, but it should
    be equivalent to previous behavior otherwise.
  - The change means that value `"A"` can be colored red in one column,
    while `"A"` can be colored blue in another column. It also means
    that numeric columns can each have their own custom color function,
    which means it can exactly match a heatmap color function for
    example, instead of relying upon the limited conditional formatting
    available in MS Excel.

- [`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md)

  - moved to its own R file for convenience

  - removed dependency on dplyr

  - changed [`require()`](https://rdrr.io/r/base/library.html) to
    `check_pkg_available()` to avoid attaching the `kableExtra` package.

  - argument `colorSub` now accepts three types of input:

    - `character` vector of colors named by column values
    - `function` that takes column values and returns colors
    - `list` named by `colnames(df)`, defining columns in each column
      using `character` or `function` as described above.

  - `colorSub` as `list` input enables colorizing `numeric` columns by
    passing a color function: `circlize::colorRamp2(colors=x, breaks=y)`

  - new argument `align` to apply alignment per column, and retains
    proper alignment for numeric columns which are colorized (something
    kableExtra does not do by default).

  - new argument `format.args` for `numeric` column formatting by
    default, and is maintained even when `numeric` columns are colorized
    (which is also not handled by kableExtra by default).

  - new argument `border_left`,`border_right` which by default show a
    light grey left border per column

  - new argument `extra_css` which by default turns off word-wrap per
    column

  - new argument `row.names` passed to
    [`kableExtra::kable()`](https://rdrr.io/pkg/knitr/man/kable.html)
    but repeated to help determine the number of columns used for the
    left border.

- [`unalpha()`](https://jmw86069.github.io/jamba/reference/unalpha.md)

  - new argument `keepNA=FALSE` which allows `NA` conversion to
    `"#FFFFFF"` by
    [`grDevices::col2rgb()`](https://rdrr.io/r/grDevices/col2rgb.html);
    with new option `keepNA=TRUE` which returns `NA`.

### new functions

- [`shadowText_options()`](https://jmw86069.github.io/jamba/reference/shadowText_options.md)

  - intended to help manage shadowText settings which are stored as
    [`options()`](https://rdrr.io/r/base/options.html).

## jamba 0.0.95.900

### changes to existing functions

- [`groupedAxis()`](https://jmw86069.github.io/jamba/reference/groupedAxis.md)

  - now returns invisible `data.frame` with relevant coordinates.
  - new argument `do_plot=TRUE` to allow for suppressing the plot output
    and handling only the numeric values directly.
  - Added unit tests.

### bug fixes

- [`minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md)
  resolved warnings.
- [`minorLogTicksAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md)
  resolved warnings.
- [`shadowText()`](https://jmw86069.github.io/jamba/reference/shadowText.md)
  resolved warnings.
- [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md)
  resolved warnings in the examples.
- [`rbindList()`](https://jmw86069.github.io/jamba/reference/rbindList.md)
  removed base R example that produced a warning.
- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  removed warnings.

## jamba version 0.0.94.900

### changes to existing functions

- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)

  - Fixed regression where it was printing lots of verbose info.

- [`getColorRamp()`](https://jmw86069.github.io/jamba/reference/getColorRamp.md)

  - New argument `dex` as replacement for `gradientWtFactor`, reflecting
    the same argument in
    [`color2gradient()`](https://jmw86069.github.io/jamba/reference/color2gradient.md).
    This argument is only used when a single color is provided, in order
    to create a color gradient using that color. The argument `dex`
    improves the color tone, which more closely matches the original
    color without becoming too de-saturated during the process.
  - Change to existing argument: `gradientWtFactor=NULL` so this
    argument is ignored, in favor of new argument `dex=1`.
  - Updated unit tests accordingly.

- [`color2gradient()`](https://jmw86069.github.io/jamba/reference/color2gradient.md)

  - argument `dex` now accepts values at or below zero, by converting
    them to fractions.
  - Added unit tests.

## jamba version 0.0.93.900

### bug fixes

- [`grepls()`](https://jmw86069.github.io/jamba/reference/grepls.md) now
  properly works with regular expressions when searching a `list` of
  package functions. Also added unit tests to confirm this use case.
- Fixed test cases for
  [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md) and
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  functions.
- [`getColorRamp()`](https://jmw86069.github.io/jamba/reference/getColorRamp.md)
  fixed rare edge case with a named palette, using n=NULL (so it returns
  a color function), and the palette was reversed.

### new functions

- [`gsubs()`](https://jmw86069.github.io/jamba/reference/gsubs.md)

  - This function is being moved from `multienrichjam::gsubs()` into
    jamba. During the process, it was noted that input `x` could not be
    a `list`, which is inconsistent with other functions such as
    [`lengths()`](https://rdrr.io/r/base/lengths.html). It now processes
    `list` input either iteratively (if input is a nested list), or as
    unlisted subsets for optimal performance.

### changes to existing functions

- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)

  - New argument `asp` with optional ability to define a fixed aspect
    ratio. When enabled, the plot `xlim` and `ylim` behaves slightly
    differently, but consistent with
    [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
    [`plot.default()`](https://rdrr.io/r/graphics/plot.default.html),
    and [`plot.window()`](https://rdrr.io/r/graphics/plot.window.html),
    which include at least `xlim` and `ylim` but expand one axis if
    needed in order to obtain the fixed aspect ratio.
  - When `add=TRUE` the `xlim` and `ylim` values by default will use the
    plot device range, instead of the data value range. This change
    makes the density consistently calculated for the plot area used for
    display.
  - The method for drawing background fill color `fillBackground=TRUE`
    changed from using `usrPar()` to using
    [`grid::grid.rect()`](https://rdrr.io/r/grid/grid.rect.html).
    However,
    [`grid::grid.rect()`](https://rdrr.io/r/grid/grid.rect.html)
    apparently honors the plot device itself only when
    `par("xpd"=FALSE)` and after
    [`abline()`](https://rdrr.io/r/graphics/abline.html) is called with
    non-empty and non-infinite value for `h` or `v`. Weird. Nonetheless,
    it causes
    [`grid::grid.rect()`](https://rdrr.io/r/grid/grid.rect.html) to crop
    output to the plot device, which has the benefit of resizing to the
    plot device when `asp` aspect ratio is defined, when the device is
    resized. Normally
    [`usrBox()`](https://jmw86069.github.io/jamba/reference/usrBox.md)
    draws a box with fixed coordinates, and with `asp` is defined the
    `xlim` and `ylim` will change with the plot device is resized. In
    future, if the
    [`grid::grid.rect()`](https://rdrr.io/r/grid/grid.rect.html) method
    is problematic, the method will need to revert back to using
    `usrPar()`.

## jamba version 0.0.92.900

### bug fixes

- [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md)

  - error was thrown when trying to display
    [`circlize::colorRamp2()`](https://rdrr.io/pkg/circlize/man/colorRamp2.html)
    color function, a recently updated version of `circlize` now stores
    colors internally as R hex colors, instead of previous versions
    which stored `numeric` matrix of r,g,b color values.
  - The function was updated to detect whether colors defined for color
    breaks in
    [`circlize::colorRamp2()`](https://rdrr.io/pkg/circlize/man/colorRamp2.html)
    data are `numeric` matrix of r,g,b values, or `character` vector of
    R colors. It should therefore be robust to earlier or newer versions
    of
    [`circlize:: colorRamp2()`](https://rdrr.io/pkg/circlize/man/colorRamp2.html)
    color functions.

### changes to existing functions

- [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md)

  - new argument `doPlot=TRUE` which allows disabling the graphical
    output, mainly motivated by the desire to create unit tests.

- [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)

  - New arguments `startRow`, `startCol` to define starting row and
    column, repectively, for a new worksheet.
  - Argument `doFilter=FALSE` was being ignored, since the default
    worksheet created by
    [`openxlsx::writeDataTable()`](https://rdrr.io/pkg/openxlsx/man/writeDataTable.html)
    already enabled column filtering by default. This argument is
    applied properly now.
  - Column widths are now applied in one section of code, after the
    various column formatting options, which may improve column width
    “auto” when formatting changes the visible width of values in each
    column.
  - Help docs were updated and formatted for better markdown style.

- [`readOpenxlsx()`](https://jmw86069.github.io/jamba/reference/readOpenxlsx.md)

  - new arguments `startCol` consistent with the same argument in
    [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md),
    and `cols` used to specify an exact vector of columns. The arguments
    allow loading data starting at a specific column or for specific
    columns in each Excel worksheet.
  - Added simple unit tests.

- Added simple unit tests to verify that a `data.frame` saved by
  [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
  and re-loaded by
  [`readOpenxlsx()`](https://jmw86069.github.io/jamba/reference/readOpenxlsx.md),
  optionally using `startRow` and `startCol`, will reproduce the
  identical input `data.frame`.

## jamba version 0.0.91.900

### bug fixes on R-4.2.3

- Fixed errors caused by `if(class(x) %in% x)` since `class(x)` can have
  \>1 value, an error on R-4 and only a warning on R-3.

  - notably
    [`imageByColors()`](https://jmw86069.github.io/jamba/reference/imageByColors.md)
    and
    [`handleArgsText()`](https://jmw86069.github.io/jamba/reference/handleArgsText.md)
    which called iteratively by the argument-printing utility
    [`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md).

- [`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md)

  - When using `cache.lazy=FALSE` the cache files did not save `.rdx`
    and `.rdb` files, causing this function to fail. In that case the
    `.RData` files can be loaded but not using
    [`lazyLoad()`](https://rdrr.io/r/base/lazyload.html) so they are
    loaded directly into the environment `envir`. See changes below.

### changes to existing functions

- [`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md)

  - new argument options: `file_sort=c("globals", "objects")` which
    defines the file order by default to use the order they appear in
    the RMarkdown index files `"__globals"` or `"__objects"`. Both files
    should be present, and should be identical, but the option is there
    to choose one or the other.

  - Using globals is the new default, as it is more reliable than using
    file creation time:

    - the RMarkdown chunks can be re-ordered, which would cause the
      cache file creation to be out of order from the RMarkdown document
    - also chunks can be removed, and because cache files are not
      forcibly removed in that case, the new mechanism prevents loading
      un-necessary cache files which may corrupt the final environment.

  - new argument: `preferred_load_types=c("lazyLoad", "load")` to help
    limit the mechanism used to load cache objects. When `.rdx/.rdb`
    files exists, the default is to use
    [`lazyLoad()`](https://rdrr.io/r/base/lazyload.html). When they do
    not exist, `.RData` files typically always exist, and can be loaded
    with [`load()`](https://rdrr.io/r/base/load.html). The option now
    exists to prevent
    [`lazyLoad()`](https://rdrr.io/r/base/lazyload.html) and to use
    [`load()`](https://rdrr.io/r/base/load.html) instead. Subtle
    difference in how R objects are stored and re-used.

- numerous functions had argument help text updated for clarity.

- [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md),
  [`ssdim()`](https://jmw86069.github.io/jamba/reference/sdim.md),
  [`sdima()`](https://jmw86069.github.io/jamba/reference/sdim.md),
  [`ssdima()`](https://jmw86069.github.io/jamba/reference/sdim.md) help
  docs were combined.

## jamba version 0.0.90.900

- Added MIT license.
- Added testthat unit testing.

### new functions

- [`middle()`](https://jmw86069.github.io/jamba/reference/middle.md)

  - Similar to [`head()`](https://rdrr.io/r/utils/head.html) and
    [`tail()`](https://rdrr.io/r/utils/head.html), except it shows the
    middle. Actually by default it shows evenly spaced entries from
    beginning, middle, and end.

### bug fixes

- [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md),
  [`cPasteS()`](https://jmw86069.github.io/jamba/reference/cPaste.md),
  [`cPasteSU()`](https://jmw86069.github.io/jamba/reference/cPaste.md)

  - Fixed error when one or more entries were `NULL`, output now retains
    `NULL` without error.
  - Added tests to cover behavior with NULL entries, including retaining
    proper names or absence of `names(x)` for input data.

### changes to existing functions

- [`getColorRamp()`](https://jmw86069.github.io/jamba/reference/getColorRamp.md)

  - accepts `function` input from
    [`circlize::colorRamp2()`](https://rdrr.io/pkg/circlize/man/colorRamp2.html)
  - accepts `character` function name with optional package prefix.

- [`coordPresets()`](https://jmw86069.github.io/jamba/reference/coordPresets.md),
  [`drawLabels()`](https://jmw86069.github.io/jamba/reference/drawLabels.md)

  - new argument `preset_type` to position labels either at the plot
    frame (default), or `preset_type="figure"` will position labels
    relative to the margin around each figure.

- [`nullPlot()`](https://jmw86069.github.io/jamba/reference/nullPlot.md)

  - new argument `marginUnit` to specify whether to display margin units
    in lines `marginUnit="lines"` (default) or inches
    `marginUnit="inches"`.
  - Margin and Outer Margin are both printed in summary four-value form,
    and along each side. Outer margin values are hidden when zero.
  - Outer margin dotted dark green line has width=3 to make it distinct
    from the margin dotted line with width=1.

## jamba version 0.0.89.900

### bug fixes

- [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  and
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md)
  used similar but non-identical logic when applying `ignore.case=TRUE`
  and `useCaseTiebreak=TRUE`.

  - Code and logic were consolidated into
    [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md)
    (as it should have been), and the logic was corrected to fix weird
    edge cases of internal capitalization within a mixed
    character/numeric string.
  - Logic essentially sorts without regard to case, then applies a
    tiebreaker using case at the end. This is similar logic used by
    default [`sort()`](https://rdrr.io/r/base/sort.html), though I admit
    it feels weird. Default [`sort()`](https://rdrr.io/r/base/sort.html)
    returns `c("aardvark", "Aardvark", "abacus", "Abacus")`.
  - Code consolidation should speed
    [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
    by almost two-fold for default settings, avoiding almost duplicating
    the same sort logic just to apply the case-sensitive tiebreaker. I
    mentioned that time hit in previous NEWS.

- [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)

  - Optimized to handle mixed-class, and nested or simple list.

  - When the input `list` contains multiple classes

    - previous behavior was to iterate the `list` otherwise class
      conversions (which help optimization) cause problems with `factor`
      and `character` types. For some reason, when calling `unlist(x)`
      and `x` contains `factor` and `character`, the output is converted
      to `character` (which is fine), however `factor` values are
      converted to `integer` then `character` strings of the integer
      values.
    - New behavior is to call
      [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)
      on subsets of `x` with the same class, so each subset is run with
      single-class optimization. Now instead of scaling with `length(x)`
      it scales with `length(class(x))`, obviously much faster.

  - When input `list` is nested, and content is all the same class, it
    runs
    [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)
    on all data en masse as is optimal.

  - When input `list` is nested, and content has different classes, each
    subset is sorted within its class, so `list` is sorted within its
    own subgroup. For inconsistently nested `list` structure, various
    branches will be sorted together, which is slightly unoptimal, but
    it does maintain the input class of each atomic vector.

  - All classes are maintained, without coersion to `character`.

- [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md)

  - Now passes `honorFactor=keepFactors` when calling
    [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md),
    which should provide a notable speed boost, in addition to
    optimizations to
    [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)
    already described.

## jamba version 0.0.88.900

Began prep for eventual CRAN release.

- added `ComplexHeatmap` to Suggests.
- edited
  [`heatmap_row_order()`](https://jmw86069.github.io/jamba/reference/heatmap_row_order.md)
  and
  [`heatmap_column_order()`](https://jmw86069.github.io/jamba/reference/heatmap_column_order.md)
  to check for ComplexHeatmap.

### changes to existing functions

- [`readOpenxlsx()`](https://jmw86069.github.io/jamba/reference/readOpenxlsx.md)

  - New default `check_header=FALSE` to sidestep issues with column
    headers that do not align with subsequent data.

- [`formatInt()`](https://jmw86069.github.io/jamba/reference/formatInt.md),
  [`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md),
  [`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md),
  [`sizeAsNum()`](https://jmw86069.github.io/jamba/reference/sizeAsNum.md),
  [`imageByColors()`](https://jmw86069.github.io/jamba/reference/imageByColors.md)

  - updated to improve how it checks `class(x)`, to allow multiple
    values.

- [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md),
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md)
  default `honorFactor=FALSE` in vector context.

  - For someone mixedSorting a vector, they expect mixedSorted output.

- [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md),
  [`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md)
  default `honorFactor=TRUE` in `data.frame` or `list` context.

  - For someone mixedSorting a `data.frame`, they expect mixedSorted
    `character` columns, and sorted factor levels. For someone
    mmixedOrdering a `list`, they expect `character` vectors to be
    mixedSorted and factors to be sorted by level.

### bug fixes

- [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md),
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md).
  [`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md),
  [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)

  - **gasp** These core functions were ignoring `honorFactor=FALSE` and
    were instead always keeping factor level order without imposing its
    own sort order.

    - The intent was to use `honorFactor=FALSE`, which was supposed to
      be consistent with previous versions of jamba. However, other
      dependent Jam functions became somewhat reliant on unintended
      behavior which represents `honorFactor=TRUE`. This behavior is
      most important for uses of
      [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
      which is expected to honor factor level order by default. This
      expectation enables other assumptions about the order of values.
      That said, specific errors have been rare or non-existent thus
      far.

    - A “problem” arises only in R versions before R-4.0.0, where
      default R-3.6.1 `options(stringsAsFactors=TRUE)` causes
      [`data.frame()`](https://rdrr.io/r/base/data.frame.html) to
      convert `character` to `factor`. During the conversion, factor
      level order is defined using vanilla
      [`sort()`](https://rdrr.io/r/base/sort.html). Therefore, using
      [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
      appears to use vanilla sort order by honoring the factor level
      order, but only in cases where the `data.frame` was created by a
      method that did not override `options(stringsAsFactors=TRUE)` for
      consistency. So the use of `honorFactor=TRUE` by
      [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
      causes an edge case of inconsistent order for some `data.frame`
      objects, dependent upon the value of
      `getOption("stringsAsFactors")`.

    - The resolution to above issues:

      - [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
        should continue to do what is expected, which in my opinion is
        to honor factor order by default; and
      - other functions which carelessly enable `stringsAsFactors=TRUE`
        to wreack havoc should themselves be fixed. “Check yourself,
        before you wreck yourself.” I shouldn’t have said that.

  - So the resolution overall is to have different defaults for
    `honorFactor`, even though it seems improper. I argue that it is
    proper:

    1.  `mixedSort(..., honorFactor=FALSE)` because someone calling
        [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
        is doing so for the benefit of mixedSort behavior, otherwise
        they can call [`sort()`](https://rdrr.io/r/base/sort.html) on a
        `factor` and get the same result as
        `mixedSort(..., honorFactor=TRUE)`. Therefore I think
        `honorFactor=FALSE` is expected for a vector.
    2.  `mixedSortDF(..., honorFactor=TRUE)` because someone calling
        [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
        is doing so for the benefit of sorting `character` columns using
        [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
        logic, otherwise their `factor` columns are assumed to have
        carefully (let’s hope) constructed factor `levels`. And since
        those factor `levels` are so dear to the analyst, their order
        will be maintained when sorting a multi-column `data.frame`. (Or
        equivalent tibble, DataFrame, etc.) Therefore I think
        `honorFactor=TRUE` is expected for a `data.frame`.
    3.  `mixedOrder(..., honorFactor=FALSE)`, due to vector context.
    4.  `mixedSorts(..., honorFactor=TRUE)`, due to list context.
    5.  `mmixedOrder(..., honorFactor=TRUE)`, due to list context.

### new functions

- `jam_calc_density()`

## jamba version 0.0.87.900

### bug fixes

- [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md) was
  not properly enforcing `na.rm=TRUE`, which caused `NA` values to be
  converted into `"NA"`. The bug also affected
  [`cPasteS()`](https://jmw86069.github.io/jamba/reference/cPaste.md),
  [`cPasteU()`](https://jmw86069.github.io/jamba/reference/cPaste.md),
  and
  [`cPasteSU()`](https://jmw86069.github.io/jamba/reference/cPaste.md),
  along with dependent functions in the `genejam` package.

  - [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md)
    now calls
    [`rmNAs()`](https://jmw86069.github.io/jamba/reference/rmNAs.md)
    when `na.rm=TRUE`, to remove `NA` values in relatively efficient
    manner across the input list `x`.
  - [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md)
    new argument `useLegacy` will optionally enable the previous code,
    which was optimized for `list` that contain vectors with identical
    classes throughout. It had the limitation that it did not preserve
    factor level order, and sometimes converted factor to `character` in
    mixed `list`.

### new function

- [`rmNAs()`](https://jmw86069.github.io/jamba/reference/rmNAs.md) is an
  extension of
  [`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md) applied
  to `list` input. It is intended to be slightly faster than iterating
  each `list` element, however for now it iterates every element that
  contains an `NA` value, in order to preserve the class of each vector.

## jamba version 0.0.86.900

### bug fixes

- A warning was being issued from
  [`rgb2col()`](https://jmw86069.github.io/jamba/reference/rgb2col.md)
  by checking if input argument `red` was empty `length(red)==0`, or NA
  `is.na(red)`. However, when `red` has `length(red)>1` the second check
  for NA results in a logical vector of `TRUE/FALSE`, only the first of
  which is used in the if statement. New versions of R invoke a warning,
  but future versions will eventually cause an error. Nonetheless the
  code was updated to check for all entries in `red` like this:
  `all(is.na(red))`.

### changes to existing functions

- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  several arguments use
  [`getOption()`](https://rdrr.io/r/base/options.html):

  - `jam.htmlOut`, `jam.comment` are `logical` and intended for usage
    inside Rmarkdown documents, to customize the output.
  - other arguments `jam.file`, `jam.append`, `jam.invert`,
    `jam.formatNumbers`, `jam.trim`, `jam.big.mark`, `jam.small.mark`
  - `htmlOut=TRUE` now properly includes `"<br/>"` to force a line break
    after the output.

- [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)
  uses iterative
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  when there are different classes contained in `x`, otherwise `factor`
  values are converted to `integer` strings.

- [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  was re-defining `factor` levels using the order of unique values,
  instead of the original `levels`. It was changed to use `levels`.

- [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md),
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md),
  [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)
  new argument.

  - New argument `honorFactor=FALSE` keeps previous legacy behavior,
    which is to perform alphanumeric sort on `character` values even in
    `factor` columns. However, now this behavior can be changed.

## jamba version 0.0.85.900

### bug fixes

- [`rowGroupMeans()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md)
  argument `rmOutliers=TRUE` along with `crossGroupMad=TRUE` incorrectly
  caused the `rowStatsFunc` to be redefined. This bug was fixed. Also,
  when `returnType="output"` it no longer returns the attributes `"n"`
  and `"nLabel"` which are intended to represent the number of non-NA
  replicates per group, and this information is not relevant to the
  input data.

## jamba version 0.0.84.900

### bug fixes

- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  bug fixed:

  - Occurred when `bgText` had length=1, or any element in the `list`
    had length=1.
  - Improperly tried to determine how to make alternating light/dark
    color without first checking the luminance of the color in this
    scenario.

### new functions

- [`color_dither()`](https://jmw86069.github.io/jamba/reference/color_dither.md)
  simply takes one color, and returns two colors that differ by a
  minimum contrast, with the same hue. Not coincidentally it substitutes
  into the
  [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  function.

### changes to existing functions

- [`cell_fun_label()`](https://jmw86069.github.io/jamba/reference/cell_fun_label.md)
  was updated to skip cells with no text, vastly improving rendering
  speed for heatmaps that contain a large proportion of blank labels.

## jamba version 0.0.83.900

### changes to existing functions

- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  was updated:

  - Argument help text was updated.
  - Argument `indent` is honored, previously ignored. When `numeric` it
    defines the number of character spaces `" "` used to indent.
  - When a color is adjusted to light,dark alternating pattern,
    [`color2gradient()`](https://jmw86069.github.io/jamba/reference/color2gradient.md)
    is called using `dex=1` instead of `gradientWtFactor=1`, which
    should apply more consistent scaling to light and dark colors.
  - New argument `dex` used to control intensity of light,dark color
    adjustment. When `dex=0` no adjustment is performed.
  - The `bgText` light,dark adjustment now uses `dex` as well, for
    consistency.
  - When `doColor=FALSE`, the output enforces conversion of `factor` to
    `character` when combining multiple values into a string, consistent
    with `doColor > 0` behavior. Should be minor effect if any.

- [`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)
  updates:

  - new argument `debug` to print the control characters
  - new default `addEscapes=NULL`, where it autodetects whether it is
    running inside RStudio, setting `addEscapes=TRUE` if not running
    inside RStudio. When `addEscapes` is defined, that setting is
    honored.

- [`drawLabels()`](https://jmw86069.github.io/jamba/reference/drawLabels.md)

  - default box color is much lighter, less saturated.

  - new argument `panelWidth` to size labels to plot panel width, mostly
    useful for labels at the top or bottom of each plot panel. This
    argument is intended to be used by the `jamma` package to place plot
    panels above each plot, at least the full width of each plot panel.

    - `panelWidth="force"` labels always equal full panel width
    - `panelWidth="minimum"` labels are at least full name width, or as
      wide as necessary for the text label
    - `panelWidth="maximum"` labels are as large as needed for text
      label but no wider than the plot panel.

## jamba version 0.0.82.900

### new functions

- [`col2hsl()`](https://jmw86069.github.io/jamba/reference/col2hsl.md)
  and
  [`hsl2col()`](https://jmw86069.github.io/jamba/reference/hsl2col.md)
  help convert colors to and from HSL color space

  - See examples for
    [`col2hsl()`](https://jmw86069.github.io/jamba/reference/col2hsl.md)
    for visual comparison between HCL and HSL
  - Note that both functions require the `farver` package, and may
    become a formal dependency of `jamba`.
  - The most dramatic selling point for HSL is that values are typically
    very consistent within Hue, without distortion during
    interconversion and adjustments. In contrast, HCL “caps” colors at
    the color gamut for each RGB channel individually, which can have
    rather dramatic effects on the hue - sometimes fundamentally
    changing the color hue more than 80 degrees! More commonly, color
    hues are shifted by HCL conversion in order to represent Chroma and
    Luminance. Sometimes the result is faithful Chroma and Luminance of
    a completely different hue, however, which is not ideal. (Note that
    other color packages such as prismatic have internal functions to
    detect whether a color is in gamut, so it can pre-adjust the color
    before conversion in the farver package. Again, not ideal.)
  - HSL does have the property that colors with the same “Lightness” are
    not the same perceived “Luminance” as with HCL. That said, for me
    this property may be advantageous, as the “yellow” hue for example
    is rather desolate when the Luminance and Chroma is toned down to
    comparable levels of blue and purple. Instead, the most saturated
    color for each hue is seen with `S=100` and `L=50`. With HCL, every
    hue has its own local Luminance values for the maximum Chroma, and
    the maximum Chroma varies substantially across all hues.

### changes to existing functions

- [`breaksByVector()`](https://jmw86069.github.io/jamba/reference/breaksByVector.md)
  was updated to:

  - enforce consistent vector names for each output, changing
    `breakPoints` and `labelPoints` to represent the unique values in
    the order they appear.
  - added `breakLengths` to the output `list` for convenience.

- [`imageByColors()`](https://jmw86069.github.io/jamba/reference/imageByColors.md)
  adjustments

  - moved `imageByColors` into its own R file `jamba-imagebycolors.R`.
  - fixed error with `doTest=TRUE`, which now returns test data
    invisibly.
  - The `cexCellnote` input is somewhat confusing and may change in
    future. To supply cex values for each column or row, use `list`
    input for `cexCellnote`, where each list element represents a column
    or row, respectively.

## jamba version 0.0.81.900

### changes to existing functions

- [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md)
  was updated to accept color `functions` in two forms:

  1.  [`circlize::colorRamp2()`](https://rdrr.io/pkg/circlize/man/colorRamp2.html)
      form, which contains attributes `"colors"` and `"breaks"` that
      define the underlying color gradient. These colors are converted
      to hex, named by `"breaks"`, then displayed.
  2.  All other `function` are expected to accept an integer number, and
      return that many colors, which are named by the integer index,
      with default length n=7.
  3.  If either above method fail, the output colors are not displayed.

- [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md)
  updates:

  - also updated to handle `par("mar")` properly, and to return `NULL`
    when input is empty, instead of throwing an error.
  - new argument `makeUnique=TRUE` to display the first unique color in
    each color vector.
  - it was moved to its own R file.

- `pasteByRowsOrdered()`

  - new argument `keepOrder=FALSE`, no change to default behavior. When
    `keepOrder=TRUE`, any `character` columns are converted to `factor`
    whose levels are defined by the order of unique existing entries,
    keeping the order provided in `x`.
  - new argument `byCols` passed to
    [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
    to define the order of column sort for factor levels. This argument
    was previously passed via `...`, but was added as a specific
    argument to make this connection more apparent.

### new functions

- [`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md)
  was added.

  - This function is a wrapper around an internal function call: within
    a function, calling another function.
  - The function allows passing named arguments in `...` to a function
    that does not permit `...` but which may match some argument names
    in `...`.
  - [`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md)
    only passes named arguments that are permitted in the subsequent
    function.
  - Form before: `some_function_call(x, ...)`
  - Form after: `call_fn_ellipsis(some_function_call, x, ...)`

## jamba version 0.0.80.900

### new functions

- [`rowGroupRmOutliers()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md)
  is a convenience function that calls
  `rowGroupMeans(..., rmOutliers=TRUE, returnType="input")`.

  - new argument `crossGroupMad=TRUE` calcualted the row MAD value using
    the median MAD per row, using non-NA and non-zero MAD values. See
    description of changes below.

### changes to existing functions

- Moved
  [`rowGroupMeans()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md)
  and
  [`rowRmMadOutliers()`](https://jmw86069.github.io/jamba/reference/rowRmMadOutliers.md)
  into a specific .R file for easier management.

- [`rowGroupMeans()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md)
  changes when `rmOutliers=TRUE`:

  - new argument `crossGroupMad=TRUE` will:

    - calculate each group MAD,
    - take the median non-zero, non-NA MAD for each row as
      `rowMadValues`
    - the median non-zero, non-NA `rowMadValues` is defined as `minDiff`
    - `rowMadValues` and `minDiff` are passed to
      [`rowRmMadOutliers()`](https://jmw86069.github.io/jamba/reference/rowRmMadOutliers.md)
      for each group, which therefore applies the same threshold to each
      group on a row, and with the same `minDiff` difference-from-median
      required.
    - These changes ensure that row MAD values are not `0` unless all
      groups have MAD=0 on the same row.
    - Also, the default `minDiff` will by default require a point to
      differ at least more than the median difference observed from
      median in each row, in each group. If all MAD=0 in all groups and
      all rows, then `minDiff=0` in which case it likely does not have
      adverse effect.

- [`rowRmMadOutliers()`](https://jmw86069.github.io/jamba/reference/rowRmMadOutliers.md)
  changes:

  - new argument `rowMadValues` can define the MAD values on each row,
    useful for setting the per-row MAD across multiple sample groups for
    example.
  - new argument `minReps` to require this many non-NA values, only
    useful when providing `rowMadValues` values, and for rows that have
    n=2 replicates.
  - `includeAttributes=TRUE` now includes attribute `"outlierDF"` which
    is a a `data.frame` with summary information for each row in input
    `x`.

## jamba version 0.0.79.900

### changes to existing functions

- [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
  changes

  - New argument `wb` to supply an existing `Workbook` object, instead
    of loading it from a file.
  - It now returns the `Workbook` object with `invisible(wb)`.
  - If `file=NULL` then output is not saved.
  - To save multiple sheets to the same file, it is substantially faster
    to build up one `Workbook` object, then save that `Workbook` one
    time at the end.
  - New argument `colWidths` will apply column widths to the `Workbook`
    internally, to avoid having to save, read, apply, and save.

- [`applyXlsxConditionalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxConditionalFormat.md),
  [`applyXlsxCategoricalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxCategoricalFormat.md),
  [`set_xlsx_colwidths()`](https://jmw86069.github.io/jamba/reference/set_xlsx_colwidths.md),
  [`set_xlsx_rowheights()`](https://jmw86069.github.io/jamba/reference/set_xlsx_rowheights.md)

  - `xlsxFile` can be a filename or `Workbook` object
  - each function returns `Workbook` invisibly

## jamba version 0.0.78.900

### changes to existing functions

- [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
  was updated:

  - `df` input objects with zero rows are returned directly.
  - Bug fixed for error thrown for objects with zero rows and no
    rownames, caused by 0.0.76.900 which inserted `NA` for empty
    rownames, but caused an error when the `df` had zero rows, since
    `NA` has length 1 and did not match the number of rows. First,
    zero-row objects are returned, no need to sort. Second, the section
    was rewritten to be more robust, now empty rownames are removed
    prior to calling
    [`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md).
  - The function now properly allows sort by character `byCols` for data
    with no colnames, useful when sorting by `"rownames"`.
  - Added examples that test and confirm the desired behavior.

### new functions

- [`plotRidges()`](https://jmw86069.github.io/jamba/reference/plotRidges.md)
  is analogous to
  [`plotPolygonDensity()`](https://jmw86069.github.io/jamba/reference/plotPolygonDensity.md)
  except that it uses `ggplot2` and `ggridges` to plot density profiles
  inside the same panel. Particularly good for comparing densities
  across columns/samples, and when there are a large number of samples.

- [`cell_fun_label()`](https://jmw86069.github.io/jamba/reference/cell_fun_label.md)
  is a wrapper function to add text labels to
  [`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html).
  It is somewhat configurable:

  - It can display a combination of one or numeric or character labels.
  - Label text color is intended to use a color that contrasts with the
    color of the heatmap cell itself, using
    [`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md).
  - It can optionally draw an outline to each cell.
  - Numeric values are optionally abbreviated, for example `1502110`
    would be displayed `1.5M`.
  - Text font size can be directly adjusted.
  - Labels can be rotated inside each cell, useful when cells are
    tall-skinny.

### other minor updates

The packagedown function categories were slightly re-ordered, and may be
re-grouped in future to help organize functions.

## jamba version 0.0.77.900

### new functions

- [`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md)
  will load the cache files stored after processing an Rmarkdown `.Rmd`
  file. It is intended to help restore the Rmarkdown data available to
  particular Rmarkdown chunks, or to restore the entire session so the
  data can be used for more analysis or visualization.

  - optionally loads into a specific `environment`
  - optionally loads until a specific Rmarkdown chunk
  - optionally loads only a given number of Rmarkdown chunks in order

### updates to existing functions

- [`color2gradient()`](https://jmw86069.github.io/jamba/reference/color2gradient.md)
  new argument default `gradientWtFactor=NULL`:

  - `gradientWtFactor` controls the intensity of the light-to-dark color
    gradient, where `gradientWtFactor >= 1` defines a broader range from
    light-to-dark, and `gradientWtFactor < 1` defines a more limited
    light-to-dark. For example, when `n=2` it there does not need to be
    as large a difference in brightness for color1 and color2.
  - New behavior assigns default `gradientWtFactor` based upon `n`.

- [`color2gradient()`](https://jmw86069.github.io/jamba/reference/color2gradient.md)
  argument `gradientWtFactor` can be supplied as a vector, so the value
  will be applied to each gradient created, in order.

## jamba version 0.0.76.900

### bug fixes

- [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
  had a bug when supplied with `matrix` that had no
  [`rownames()`](https://rdrr.io/r/base/colnames.html), it would fail
  because the intermediate `data.frame` was assigned
  `rownamesX=rownames(x)` and thus had zero length. It now assigns
  `rownamesX=rmNULL(nullValue=NA, rownames(x))` which fills the column
  with `NA` values.

## jamba version 0.0.75.900

### new functions

Ported two functions from multienrichjam.

- [`heatmap_row_order()`](https://jmw86069.github.io/jamba/reference/heatmap_row_order.md),
  [`heatmap_column_order()`](https://jmw86069.github.io/jamba/reference/heatmap_column_order.md)
  are specifically used with Bioconductor package `ComplexHeatmap`
  heatmaps. These functions were enhanced to handle several scenarios:
  presence of absence of `row_groups`, `column_groups`; with and without
  [`rownames()`](https://rdrr.io/r/base/colnames.html),
  [`colnames()`](https://rdrr.io/r/base/colnames.html) in the data
  matrix; with and without specific `row_labels`, `column_labels`.

## jamba version 0.0.74.900

### new function

- `diff_functions()` is a simple utility function intended to compare
  the text in two functions, motivated while migrating functions into R
  packages, while needing to make changes in the old function source,
  propagating to the new function source. Currently the function
  requires access to commandline tool `"diff"` which likely limits this
  function to linux-like operating systems, MS Windows System Linux
  (WSL), or Cygwin on MS Windows.

## jamba version 0.0.73.900

### enhancements

- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  was improperly defining `xlab` and `ylab` when supplied with a
  `data.frame` or `matrix` with colnames. The steps now closely match
  [`graphics::plot.default()`](https://rdrr.io/r/graphics/plot.default.html).
- [`noiseFloor()`](https://jmw86069.github.io/jamba/reference/noiseFloor.md)
  was not checking for `minimum` to be `NULL`, though this error has not
  occurred myself, that step is now performed as it would have thrown an
  error.

## jamba version 0.0.72.900

### bug fixes

- [`renameColumn()`](https://jmw86069.github.io/jamba/reference/renameColumn.md)
  when the `to` values were supplied as `factor` values, they were
  coerced by R into `integer` and therefore lost the character string
  values provided. The workaround is to coerce `to` with
  [`as.character()`](https://rdrr.io/r/base/character.html) which is
  straightforward for processes outside
  [`renameColumn()`](https://jmw86069.github.io/jamba/reference/renameColumn.md),
  however the expected behavior is certainly to maintain the character
  string, so this function has been updated accordingly. The situation
  tends to happen when the column renaming is stored inside a
  `data.frame` and the rename “to” column type `factor` is not checked
  beforehand. It is a rare scenario, but warrants a fix. In addition,
  [`renameColumn()`](https://jmw86069.github.io/jamba/reference/renameColumn.md)
  also operates on `GRanges` and related genomic ranges objects, and
  that step requires `IRanges::values()` generic function. The
  `values()` functions were updated to include the proper package
  prefix, in the event those packages had not already been loaded. This
  function now explicitly requires `length(from) == length(to)`. The
  help text was updated with more information.

## jamba version 0.0.71.900

### changes to existing functions

- [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
  argument default `useRownames=TRUE` was changed to `useRownames=FALSE`
  to protect against extremely slow sort behavior with large `nrow`
  data, which would previously by default sort all rownames even when
  not required. The new behavior only sorts the columns specified, and
  can enable rownames as a tie-breaker with `useRownames=TRUE` or by
  including `byCols=0`.
- [`getColorRamp()`](https://jmw86069.github.io/jamba/reference/getColorRamp.md)
  now accepts gradient names supplied by
  [`colorjam::jam_linear`](https://jmw86069.github.io/colorjam/reference/jam_linear.html)
  and
  [`colorjam::jam_divergent`](https://jmw86069.github.io/colorjam/reference/jam_divergent.html),
  when the `"colorjam"` R package is available. The linear gradients
  differ mainly in subtle ways, emphasizing more color saturation in the
  mid-range values, motivated by their use in coverage heatmaps.

## jamba version 0.0.70.900

### bug fixes

- Not really a bug, but
  [`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md)
  was apparently enabling verbose output by default by using
  `verbose - 1`, and how did I not realize that `if (-1) cat ("yes")`
  would print `"yes"`? Geez. This bug also caused
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  and
  [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
  to print extremely verbose output by default.

## jamba version 0.0.69.900

Reminder to myself to use `options(pkgdown.internet=FALSE)` when behind
VPN. And sometimes when not behind VPN…

### new functions

- `sizeToNum()` performs the opposite function as
  [`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md), it
  takes an abbreviated size as a character string, and converts to
  `numeric` value.

### bug fixes

- [`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md)
  was updated to fix a bug when sorting numbers with class
  `"integer64"`, which is defined by
  [`bit64::integer64`](https://rdrr.io/pkg/bit64/man/bit64-package.html).
  These values were produced by
  [`openxlsx::read.xlsx()`](https://rdrr.io/pkg/openxlsx/man/read.xlsx.html)
  that contained large integer values, and were previously ignored by
  [`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md).
  The change also fixes a bug in
  [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md),
  however this bug did not affect
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  and
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md).
  The conditional now tests
  [`is.numeric()`](https://rdrr.io/r/base/numeric.html) which returns
  `TRUE` for
  [`bit64::integer64`](https://rdrr.io/pkg/bit64/man/bit64-package.html)
  types, and hopefully this logic will work for future large numeric
  types.
- [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  fixed an issue where `ignore.case=TRUE` caused factors to be sorted
  alphanumerically instead of by factor level. The new and intended
  behavior is to sort by the unique uppercase factor levels, otherwise
  maintaining the factor level order.
- [`rbindList()`](https://jmw86069.github.io/jamba/reference/rbindList.md)
  was updated to change the way `returnDF=TRUE` is handled. It now
  simply calls `data.frame(x, check.names=FALSE)`, with no further
  validation. Previously this option called a function
  `unlistDataFrame()` that did not get ported into `jamba`, and probably
  will not.

### changes to existing functions

- [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
  was enhanced to allow row name sort, using these formats:
  `byCols="rownames"` or `byCols="row.names"` or `byCols=c(0)`. Reverse
  row name sort can be accomplished by `byCols="-rownames"` or
  `byCols="-row.names"` or `byCols=c(-0.01)`. When `byCols` contains
  `numeric` values, the sign is taken first so any negative values will
  reverse the sort, then values are rounded to integer values. So
  `byCols=-0.1` will reverse sort by row names, useful when it is more
  convenient to provide `byCols` as a `numeric` vector than a
  `character` vector.

- [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  was refactored to simplify the logic, and to correct small issues, for
  example when `ignore.case=TRUE` and `x` is a factor, the previous
  behavior would call [`toupper()`](https://rdrr.io/r/base/chartr.html)
  which converts `x` to `character`, and which is sorted as such. The
  new intended behavior is to sort by `unique(toupper(levels(x)))` to
  maintain factor level order as defined. The new logic defines a
  transform function for `x`, whose output is passed to
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md).
  For example, when `sortByName=TRUE` the transform function passes
  `names(x)` to
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md).
  Otherwise, there is only one call to
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md)
  and no data is copied during the process, which should be reasonably
  memory-efficient for large sort operations. Frankly, it needs one more
  refactor to push all the logic into
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md).

- [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md),
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md),
  and
  [`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md)
  had arguments extended to represent all arguments in
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md),
  so the sort customizations are properly passed between functions.

### File re-organization

Some R functions were moved into separate files for easier management.
More work should be done over time to reduce the number of functions in
each file. Some files have 15 to 31 functions!

- [`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md) was
  moved from `jamba.R` into `jamba-size.R`.
- [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  functions were moved into `jamba-mixedSort.R`.

## jamba version 0.0.68.900

### changes to existing functions

- [`lldf()`](https://jmw86069.github.io/jamba/reference/lldf.md) new
  argument `use_utils_objectsize=TRUE` to prefer
  [`utils::object.size()`](https://rdrr.io/r/utils/object.size.html)
  instead of `pryr::object_size()` after observing some cases where
  object sizes were vastly over-stated for unusual Bioconductor object
  types. It is unclear to me exactly how the memory usage is determined,
  and which function is correct, so for now the two alternatives will
  remain in
  [`lldf()`](https://jmw86069.github.io/jamba/reference/lldf.md).

## jamba version 0.0.67.900

### changes to existing functions

- [`readOpenxlsx()`](https://jmw86069.github.io/jamba/reference/readOpenxlsx.md)
  was updated to fix a small bug when loading multiple sheets in one
  pass. It should now work as expected to run
  [`readOpenxlsx()`](https://jmw86069.github.io/jamba/reference/readOpenxlsx.md)
  on any xlsx file, and return a `list` of `data.frame` objects
  representing each worksheet in the xlsx file.

- [`readOpenxlsx()`](https://jmw86069.github.io/jamba/reference/readOpenxlsx.md)
  new arguments `check_header` and `check_header_n` are intended to
  check for header rows, recognized as rows with a different number of
  columns than subsequent rows of data in each sheet. When
  `check_header=TRUE` it will check the first `check_header_n` rows to
  determine [`ncol()`](https://rdrr.io/r/base/nrow.html) and
  [`sclass()`](https://jmw86069.github.io/jamba/reference/sclass.md) on
  each row loaded individually. When header rows are detected, the first
  value of the first row is assigned to column `dimnames` of the
  `data.frame` returned, and the full header data is included as an
  attribute `attr(df, "header")`. This method is intended to help when
  loading multiple worksheets, and none, some, or all worksheets may
  have header rows.

- Note this methodology is being tested currently, to debug edge cases
  where data is sometimes detected to have fewer columns in some rows
  than others. The intent is to expect `skipEmptyCols=FALSE` to return
  all columns even when there is no data present, for consistent
  [`ncol()`](https://rdrr.io/r/base/nrow.html) from data rows, and fewer
  columns for header rows. The logic may be updated over time.

- [`readOpenxlsx()`](https://jmw86069.github.io/jamba/reference/readOpenxlsx.md)
  was updated to apply `skipEmptyCols=FALSE` by default for each step,
  to avoid inconsistencies especially when the column headers are not
  defined for all columns, or data is not present in all columns with
  column headers.

## jamba version 0.0.66.900

### changes to existing functions

- [`rowGroupMeans()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md)
  was updated to remove a silent dependency on `matrixStats` and now
  properly checks the dependency, and calls with prefix
  [`matrixStats::rowMedians()`](https://rdrr.io/pkg/matrixStats/man/rowMedians.html)
  and
  [`matrixStats::rowMads()`](https://rdrr.io/pkg/matrixStats/man/rowSds.html)
  as needed.
- [`applyXlsxCategoricalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxCategoricalFormat.md)
  was updated to streamline the categorical color matching, though not
  by much for large files. Will revisit and improve speed eventually.
- Fixed some help documents not using markdown format when referring to
  other package functions.

### new functions

- [`igrepl()`](https://jmw86069.github.io/jamba/reference/igrepl.md) is
  a convenient wrapper for `grepl(..., ignore-case=TRUE)`

## jamba version 0.0.65.900

### changes to existing functions

- [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
  new argument `wrapCells=FALSE` changes the previous behavior so that
  cells are not word-wrapped by default. Previously, when column width
  did not accomodate the text, cells would be tall - and sometimes
  “super-tall”. The function
  [`set_xlsx_rowheights()`](https://jmw86069.github.io/jamba/reference/set_xlsx_rowheights.md)
  can be used to fix this situation, however that process can be quite
  slow for large Excel worksheets.

## jamba version 0.0.64.900

### changes to existing functions

- [`provigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md)
  argument `maxValues` had not beedn implemented properly when applying
  a limit to returned values per grep pattern. This implementation has
  been corrected. Examples have been updated to demonstrate its utility.

### new functions

- [`heads()`](https://jmw86069.github.io/jamba/reference/heads.md)
  applies [`head()`](https://rdrr.io/r/utils/head.html) across a list of
  atomic vectors, using a one-step style that is notably faster than
  `lapply(x, head, n)` especially for long lists. Also it can apply a
  vector of `n` values if needed. Its main utility is for
  [`provigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md)
  with argument `maxValues`.
- [`proigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md)
  was added as a lightweight alias to `provigrep(..., value=FALSE)`
  which returns the index positions rather than the values.
- `match_unique()` matches unique vector values to the full vector. I
  found myself performing this step so many times I thought a function
  might feel better. It may also be equivalent to
  `which(!duplicated(x))` - but again, that doesn’t feel easy enough. I
  guess.

## jamba version 0.0.63.900

### new function

- [`lldf()`](https://jmw86069.github.io/jamba/reference/lldf.md) is a
  lightweight extension to `utils::ls()` that returns a `data.frame`
  with object name and size, sorted largest to smallest. I found myself
  using this function several times after becoming more aware of object
  sizes while using
  [`jamsession::save_jamsession()`](https://jmw86069.github.io/jamsession/reference/save_jamsession.html).

### changes to existing functions

- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  default argument `bandwidthN=NULL` which changes the default bandwidth
  to use `bwpi` for the per-inch calculation of the bandwidth that can
  be reasonably displayed given the output size. Previous default was
  `bandwidthN=300`. Note that the output density calculation is
  dependent upon the display size, which can affect the results.
- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  now honors `xlab` and `ylab`, and when those are not provided, it uses
  base R convention and applies `deparse(substitute(x))` to determine a
  suitable default label.

## jamba version 0.0.62.900

### changes to existing functions

- [`shadowText()`](https://jmw86069.github.io/jamba/reference/shadowText.md)
  new argument `shadowOrder` to control the rendering of shadows and
  labels for a vector of labels: `shadowOrder="each"` renders each
  shadow then each label in order, so that subsequent shadows will
  overlap previously rendered labels; `shadowOrder="all"` renders all
  shadows at once then all labels, so that shadows will never overlap
  labels. Very minor issue, mostly affecting display of labels that
  slightly overlap - see examples in
  [`shadowText()`](https://jmw86069.github.io/jamba/reference/shadowText.md).
- [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md)
  fixed issue in first time processing, which was wrongly defining
  Crange and Lrange for future use even inside Rstudio. (Minor issue
  that would only affect text labels with light colors on a light
  background.)

## jamba version 0.0.61.900

### changes to existing functions

- `fix_matrix_ratio()` had a bug in secondary logic referring to
  non-existent object `y`. This function is used as an optional backend
  for `imageDefault` when `useRaster=TRUE` to provide reasonable default
  matrix adjustment to reduce or prevent visual distortion when image
  interpolation is performed on matrices with non-1:1 ratio of rows and
  columns.

## jamba version 0.0.60.900

### new functions

- [`readOpenxlsx()`](https://jmw86069.github.io/jamba/reference/readOpenxlsx.md)
  is the reciprocal to
  [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
  that differs from
  [`openxlsx::read.xlsx()`](https://rdrr.io/pkg/openxlsx/man/read.xlsx.html)
  mainly in that it loads multiple sheets at a time, and does not mangle
  column names when `check.names=FALSE`. The `openxlsx` method behaves
  as if `check.names=TRUE` in all cases, as of 01-Dec-2020.

## jamba version 0.0.59.900

### changes to existing functions

- [`getColorRamp()`](https://jmw86069.github.io/jamba/reference/getColorRamp.md)
  argument `defaultBaseColor="grey99"` from the previous
  `defaultBaseColor="grey95"` which just felt too dark.
- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  new argument `col` passed to
  [`smoothScatterJam()`](https://jmw86069.github.io/jamba/reference/smoothScatterJam.md)
  to be compatible with
  [`smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html),
  this defines point color when `nrpoints` is non-zero. Apparently not
  defining this argument allowed R to do fuzzy argument name matching,
  causing conflict with `colramp`.
- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  new argument `expand` will expand the x-axis and y-axis ranges by this
  amount. The default `expand=c(0.04, 0.04)` mimics the default setting
  for R base plots. The help text was updated.
- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  now uses
  [`grDevices::xy.coords()`](https://rdrr.io/r/grDevices/xy.coords.html)
  to define coordinates, avoiding internal logic that essentially did
  the same thing. This change should be silent. This change does remove
  the warnings that were produced with input supplied as a single
  numeric vector.

### bug fixes

- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  argument `binpi` was setting the bins per inch identically for x and y
  axes, using the maximum size – when it can and now does handle each
  axis independently. Now the output pixels should be square, rather
  than being influenced by the ratio of the plot device.

## jamba version 0.0.58.900

### changes to existing functions

- [`getColorRamp()`](https://jmw86069.github.io/jamba/reference/getColorRamp.md)
  was updated for single-color input and `defaultBaseColor`. It now
  properly creates a color gradient from light-to-dark, or dark-to-light
  based upon the relative brightness of `color` and `defaultBaseColor`.

## jamba version 0.0.57.900

### changes to existing functions

Updates to
[`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md)
mainly affect colorized console text output, used by
[`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
which calls
[`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md).
This function is also useful to adjust a color or vector of colors for
contrast on a light or dark background.

- [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md)
  argument has new default `lightMode=NULL` and slightly different, more
  intuitive behavior. When `lightMode` is defined (`TRUE` or `FALSE`)
  then it over-rides existing values, and uses the corresponding
  `lightMode` setting directly. Otherwise, when `lightMode=NULL` any
  existing `Crange` and `Lrange` values are used. If none are defined,
  then
  [`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md)
  is called, then default values are used.
- Functions that use
  [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md)
  were updated so the argument default `lightMode=NULL` is used. When
  those functions are called with `lightMode=TRUE` or `lightMode=FALSE`
  the text output will be adjusted. For example:

&nbsp;

    printDebug("yellow", lightMode=FALSE)
    printDebug("yellow", lightMode=TRUE)

Functions affected: \*
[`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md)
\*
[`applyCLrange()`](https://jmw86069.github.io/jamba/reference/applyCLrange.md)
\*
[`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md)
\*
[`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
\*
[`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md)
\* [`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md)

## jamba version 0.0.56.900

### bug fixes

- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  failed on class `"package_version"` ultimately because
  [`unnestList()`](https://jmw86069.github.io/jamba/reference/unnestList.md)
  could not unlist this object type. The “package_version” class is a
  list, e.g. `is.list(packageVersion("base"))` is `TRUE`. However,
  accessing the first item, e.g. `x[[1]]` always returns the full object
  unchanged… which is not expected behavior for a list. This class was
  added to `stopClasses`.
- [`unnestList()`](https://jmw86069.github.io/jamba/reference/unnestList.md)
  was refactored to handle nested lists with and without names in
  various combinations, and to use
  [`tryCatch()`](https://rdrr.io/r/base/conditions.html) as a final
  protection from infinite recursion, when list-list objects do not
  behave like list objects (as is the case with the `"package_version"`
  class.)
- `applyCLranges()` was updated to remove the warning message for
  argument `fixYellow` that read
  `"the condition has length > 1 and only the first element will be used"`.
  The function now handles vector input for `fixYellow` and extends the
  vector to the length of `x`.

### changes to existing functions

- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  now correctly determines the plot aspect ratio after the plot is
  initiated. The situation occurs when using
  [`graphics::layout()`](https://rdrr.io/r/graphics/layout.html) which
  does not update the [`par()`](https://rdrr.io/r/graphics/par.html)
  settings until the plot is created. Now a blank plot is initiated
  before determining aspect ratio, which allows proper calculation of
  the 2D density plot.

### refactoring of plotSmoothScatter

- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  new argument `binpi` defines the `nbin` number of displayed bins based
  upon plot panel size. The default `binpi=50` defines 50 display bins
  per inch. This value will adjust accordingly for large and small
  figures, based upon the physical size of the figure in inches.
- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  argument `nbin` new default is `nbin=NULL`, which then uses `binpi` to
  calculate `nbin`. To use the previous style, set `nbin=256` which will
  ignore `binpi`.
- [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  new argument `bwpi` defines `bandwidthN` based upon the plot panel
  size. The default `bwpi=NULL` does not implement this scaling by
  default, instead `bandwidthN=300` is still the default. The benefit of
  using `bwpi` is mostly in speed of calculating 2D density for
  multi-panel plots where detail would already be lost during display.

#### plotSmoothScatter commentary

[`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
uses two arguments to determine how data is plotted:

- `bandwidthN` defines the number of bandwidth divisions on the x- and
  y-axes used to calculate the 2D density. Higher `bandwidthN`
  calculates greater detail in the 2D density – these values are all
  typically higher than
  [`graphics::smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html),
  and this is still the driving reason to use `plotSmoothScatter` and
  not
  [`smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html).
- `nbin` defines the number of visible pixels used to display the
  calculated 2D density. Higher `nbin` creates visually more detailed
  representation of the 2D density.

Usually `nbin` and `bandwidthN` values are similar to each other, since
it conceptually makes sense to calculate 2D density at roughly the
resolution used to display the results. These arguments are adjustable
and are still valid for most purposes.

So why the change? The short summary is that multi-panel plots have
fewer pixels being displayed, and I wanted a convenient method to reduce
the number of pixels (`nbin`) displayed in each panel. (Truth be told,
while working from home creating plots on remote servers, I noticed just
how long multi-panel plots take to render. Most of that time is spent
rendering detail never seen during display.)

After some extended testing, I concluded that bandwidth should remain
relatively constant regardless of the display pixels, in order to
maintain consistent 2D density despite the image size. (Conceptually, it
makes sense that the image display size should not affect underlying
calculation of the data to be displayed.) However, the number of
displayed pixels should be reduced roughly proportional to the size of
the plot panel, which solves the issue I was having of plotting data
with far more detail than could be visually rendered, making plots
larger, take longer to display, and creating larger saved file sizes.

Also, the resolution of the resulting plot can now be adjusted relative
to the output plot size, consistent with computer monitor resolution
(dots per inch dpi), and printed paper output dpi.

## jamba version 0.0.55.900

### changes to existing functions

- [`applyXlsxConditionalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxConditionalFormat.md)
  and
  [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
  were updated to make default colors in `numStyle`, `intStyle`,
  `pvalueStyle` slightly more colorful, while also being less dark, so
  Excel default black text has better visible contrast. (Why can Excel
  not apply conditional format rules to text font color?)
- [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
  format for P-values `pvalueFormat` formats values above 0.01 as
  decimal value such as `0.022` or `0.157`, P-values below 0.01 are
  exponential notation such as `2.31E-10`. (I wish R could avoid
  formatting things like `1.1e+00` and `1.0e+01` without using a custom
  function.)
- [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
  was updated with a more comprehensive example.
- `applyCategoricalFormat()` has a simple working example.
- [`applyXlsxConditionalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxConditionalFormat.md)
  has a simple working example.

### new functions

- [`set_xlsx_rowheights()`](https://jmw86069.github.io/jamba/reference/set_xlsx_rowheights.md)
  and
  [`set_xlsx_colwidths()`](https://jmw86069.github.io/jamba/reference/set_xlsx_colwidths.md)
  are light wrappers to the very useful functions
  [`openxlsx::setRowHeights()`](https://rdrr.io/pkg/openxlsx/man/setRowHeights.html)
  and
  [`openxlsx::setColWidths()`](https://rdrr.io/pkg/openxlsx/man/setColWidths.html).
  They simply open the workbook, apply the values to the appropriate
  sheet, then save the workbook.

## jamba version 0.0.54.900

### changes to existing functions

- [`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)
  by default wraps the color sequence inside proper ansi escape
  sequences, which helps the console properly apply word wrapping by
  ignoring color sequences in the estimate of character count per line.

## jamba version 0.0.53.900

### changes to existing functions

- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  fixed a visual glitch caused by using
  [`gsub()`](https://rdrr.io/r/base/grep.html) to remove trailing `sep`
  characters from vector elements before adding the `sep` itself,
  without removing ANSI-encoding from the `sep` string, sometimes
  removing trailing zero from numeric strings but only in the middle of
  a multi-element vector. Now the `sep` is appended only using
  [`paste0()`](https://rdrr.io/r/base/paste.html) and no trailing `sep`
  characters are checked nor removed. This behavior is probably better
  since it should properly represent the input content.
- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  was modified to fix a longstanding visual glitch that would not fully
  reset inverted ANSI colors that span multiple lines. If the output
  uses color, a trailing `crayon::reset("")` is added to the end of each
  line.

## jamba version 0.0.52.900

### new functions

- [`jam_rapply()`](https://jmw86069.github.io/jamba/reference/jam_rapply.md)
  is a lightweight customization to
  [`base::rapply()`](https://rdrr.io/r/base/rapply.html), specifically
  designed to keep `NULL` entries without dropping them silently. It has
  the added benefit of being able to flatten a nested input list, then
  expand back into the original nested list structure, without losing
  the `NULL` entries.

### Notes on sorting lists with factors and vectors

A theme of several updates this version has been to handle list input
that may contain `factor` and non-`factor` vectors. In fact even
handling a list of `factor` vectors is unclear at least to me. For
example, [`base::unlist()`](https://rdrr.io/r/base/unlist.html)
operating on a list of factors will define one factor vector whose
levels are literally the unique first-occurrence of each factor level.
For example `sort(factor(c("A","B"), levels=c("A","B")))` results in
`c("A", "B")`, while `sort(factor(c("A","B"), levels=c("B","A")))`
results in `c("B", "A")`. So what should be done when given:

> `list(factor(c("A","B"), levels=c("A","B")), factor(c("A","B"), levels=c("B","A"))`

In reality, most such occurrences would not happen, if factor levels are
specified, the best solution is to include the full set of factor levels
with each factor vector. But if the input contains a mixture of factor
and character, vectors, running
[`unlist()`](https://rdrr.io/r/base/unlist.html) converts factors to
integers… which is really bad. At the very least I had to handle and
prevent that scenario.

### changes to existing functions

- [`pasteByRowOrdered()`](https://jmw86069.github.io/jamba/reference/pasteByRowOrdered.md)
  was modified in two ways: new argument `na.last=TRUE` is passed to
  [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
  to allow `NA` values to be sorted at the top, which seems intuitive
  for most scenarios; also the `...` additional arguments are passed to
  [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
  which allows column sort order to be customized. Previously
  [`pasteByRowOrdered()`](https://jmw86069.github.io/jamba/reference/pasteByRowOrdered.md)
  called
  [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
  which sorted columns in the order they appear in the `data.frame`,
  however sometimes it is better to sort columns differently, while
  keeping the original column order intact. See examples for
  illustration of `na.last=TRUE`. In a two-column table with `NA` values
  in the second column, using `na.last=TRUE` (default) would yield
  factor levels `c("A_B", "A", "B_C", "B")`, while using `na.last=FALSE`
  would yield `c("A", "A_B", "B", "B_C")`. This output seems more
  intuitive, however the default in
  [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
  and
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  are `na.last=TRUE` so that default will be maintained for consistency.
- [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  argument `NAlast` is deprecated in favor of `na.last` for consistency
  with [`base::sort()`](https://rdrr.io/r/base/sort.html).

### Bug fixes

- [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md) was
  refactored to simplify internal logic, and to handle a rare edge case
  that was inconsistent with related functions. It occurs when the input
  list contains some `factor` vectors, and some non-`factor` vectors,
  and when both `doSort=TRUE` and `keepFactors=TRUE`. The
  `keepFactors=TRUE` argument tries to maintain factor levels during the
  sort, instead of using
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  across all vectors. In this case, previously the non-`factor` vectors
  were converted to factor, which by default would order factor levels
  using [`sort()`](https://rdrr.io/r/base/sort.html). Now the output is
  consistent in that the `character` vectors are converted to factors
  whose levels are ordered using
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md).
  The step occurs once across all `character` vectors, so it is fairly
  fast even for large input lists. Also, any `NULL` entries are still
  maintained and returned as `""`.
- [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)
  was updated to allow input that has no names, which was errantly
  returning `NULL`. Now names are handled gracefully, allowing no names,
  or even duplicated names, without issue.
- [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)
  now properly handles nested list input that may contain `NULL`, so
  that it returns the same structured list including `NULL` entries
  where appropriate.

### Refactoring for performance

- [`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md)
  new argument `useSimpleBioc=TRUE` will optionally enable Bioconductor
  SimpleList intermediate – mostly available to benchmark because it is
  so painfully slow it raised a red flag. The Bioconductor `S4Vectors`
  package changed how they handle `List` coersion from a simple `list`
  class, instead of using `CompressedList` it defaulted to use
  `SimpleList`. Unfortunately, `SimpleList` is roughly 6-10 times slower
  when calling [`unique()`](https://rdrr.io/r/base/unique.html), and was
  causing curious slowdowns in other functions such as `cPasteU`.
  Bioconductor does not allow coersion directly to `CompressedList`,
  instead requires coersion by specific recognized classes such as
  `"character"` with `"CharacterList"`, and `"integer"` with
  `"IntegerList"`. So because the input list is allowed to contain
  different classes, the remedy is to make a `CompressedList` for each
  recognized class, call
  [`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md)
  on that `CompressedList`, and repeat until all classes are handled.
  Any unrecognized classes are handled using `lapply(x, unique)`. See
  examples for some benchmark comparisons. When `useBioc=TRUE`, the
  optimized Bioconductor method is used. When `useBioc=FALSE`, and
  `useSimpleBioc=TRUE`, the slower Bioconductor approach is used, which
  uses `SimpleList` as an intermediate. When both `useBioc=FALSE` and
  `useSimpleBioc=FALSE`, there are two additional options:
  `keepNames=FALSE` uses `lapply(x, unique)` and loses vector names;
  `keepNames=TRUE` will invoke a special base R approach that maintains
  the original vector names for the remaining unique entries. Of these
  four approaches the timings relative to the fastest:

1.  useBioc=TRUE: 1x (fastest)
2.  useBioc=FALSE, useSimpleBioc=TRUE: 6x slower
3.  useBioc=FALSE, useSimpleBioc=FALSE, keepNames=FALSE: 1.8x slower
4.  useBioc=FALSE, useSimpleBioc=FALSE, keepNames=TRUE: 12x slower

## jamba version 0.0.51.900

### changes to existing functions

- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  has a new argument, largely silent, `doReset` which defines whether to
  apply an additional color reset to the delimiter argument `sep`.
  Previously the delimiter inherited the color from the previous text,
  but now by default the delimiter style is reset, giving more clear
  distinction between multiple values.
- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  now properly applies the Crange and Lrange values when lightMode is
  TRUE, which means it limits the brightness of text colors on a light
  background, to ensure enough contrast to read the text. When using
  [`printDebugI()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  the background color is not adjusted, and the foreground (text) color
  is set using
  [`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md),
  meaning either white or black depending upon the background
  brightness.
- [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md)
  and
  [`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md)
  were updated to apply options `"jam.Crange"`, `"jam.Lrange"` only when
  those options were `NULL`, otherwise leave them as-is.
- [`applyCLrange()`](https://jmw86069.github.io/jamba/reference/applyCLrange.md)
  now properly applies the desired `Crange` and `Lrange` values, and
  defines the appropriate options, by default only when those options
  are `NULL`.
- [`fixYellow()`](https://jmw86069.github.io/jamba/reference/fixYellow.md)
  default settings were changed to expand the range corrected “for
  green-ness”, also `fixup=TRUE` by default.
- [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md)
  argument was changed to `fixup=TRUE`, and `model="hcl"`, consistent
  with other changes in color handling.
- [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md)
  and
  [`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md)
  now use `model="hcl"` by default, which now calls the `farver` package
  if installed, otherwise they revert to using colorspace
  `model="polarLUV"` and `fixup=TRUE`. In future colorspace will likely
  be removed in favor of farver, so for now the transition has begun.
- [`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md)
  was adjusted to handle alpha transparency more accurately, the
  examples were updated and include a new example that uses
  [`drawLabels()`](https://jmw86069.github.io/jamba/reference/drawLabels.md)
  on a dark background. New argument `alphaLens` allows some fine-tuning
  of the alpha transparency blending of foreground and background
  colors.

### Other updates

Added some visuals to README.Rmd.

## jamba version 0.0.50.900

### changes to existing functions

- [`shadowText()`](https://jmw86069.github.io/jamba/reference/shadowText.md)
  was updated to use several
  [`options()`](https://rdrr.io/r/base/options.html) as default argument
  values. Although [`options()`](https://rdrr.io/r/base/options.html) is
  not always an ideal mechanism, for purely visual effects it is not in
  conflict with statistical reproducibility, and allows some overall
  graphical settings to be maintained when
  [`shadowText()`](https://jmw86069.github.io/jamba/reference/shadowText.md)
  is being called through internal
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) functions.
  For example setting `text <- jamba::shadowText` then calling a default
  base R plot function will use the
  [`jamba::shadowText()`](https://jmw86069.github.io/jamba/reference/shadowText.md)
  in place of [`text()`](https://rdrr.io/r/graphics/text.html), but
  without allowing any customization. The
  [`options()`](https://rdrr.io/r/base/options.html) mechanic allows
  some custom settings to be honored. The primary driver for these
  changes is in
  [`multienrichjam::jam_igraph()`](https://jmw86069.github.io/multienrichjam/reference/jam_igraph.html)
  which plots `igraph` objects with some customization, one option is
  `use_shadowText=TRUE` which invokes
  [`jamba::shadowText()`](https://jmw86069.github.io/jamba/reference/shadowText.md).
  It is desirable to adjust the contrast of the outline effect so it
  does not completely obscure the underlying network graph display.

## jamba version 0.0.49.900

### New experimental function

- `fix_matrix_ratio()` is used to expand a matrix to have roughly 1:1
  dimensions, up to a maximum factor per dimension. The purpose is to
  enhance [`image()`](https://rdrr.io/r/graphics/image.html),
  [`rasterImage()`](https://rdrr.io/r/graphics/rasterImage.html), and
  `grid.raster()` functions, specifically when `interpolate=TRUE` for
  images with more rows or columns than there are actual pixels to
  display the image. The `interpolate=TRUE` methods in base R do not
  account for assymetry, nor do they account for a low number of rows or
  columns. See examples for visible examples of the effect.

## jamba version 0.0.48.900

### Enhancements to README

The README.Rmd was re-organized, with more callouts of helpful `jamba`
functions. Next step will be to include visual examples.

### Minor bug fix

- [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
  was exporting files with inconsistent word wrapping by column, and
  inconsistent vertical align (`valign`) settings. The culprit was
  [`openxlsx::saveWorkbook()`](https://rdrr.io/pkg/openxlsx/man/saveWorkbook.html)
  which appears to apply default styles for cell styles that are not
  specifically encoded. Simply loading and saving a workbook would
  modify column styles. The workaround/fix was to ensure
  [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
  would specifically encode `wrapText` and `valign` for each cell, so
  that it would not be overridden by
  [`openxlsx::saveWorkbook()`](https://rdrr.io/pkg/openxlsx/man/saveWorkbook.html)
  mood.
- [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
  now forces `wrapText=TRUE` for all columns. In future the function may
  allow per-column `wrapText`.
- `applyXlsxCategorical()` has new argument `wrapText=TRUE` which is
  passed to
  [`openxlsx::createStyle()`](https://rdrr.io/pkg/openxlsx/man/createStyle.html),
  since that function requires wrapText be either `TRUE` or `FALSE`, and
  its default is `FALSE`.

## jamba version 0.0.47.900

### Enhancements

- [`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md)
  no longer calls `par("bg")` to determine the background color, unless
  a graphics device is already open. This change will prevent a graphics
  device from being opened in somewhat random situations. The argument
  `bg` is only used when input `colors` have alpha transparency. When
  `bg` is not supplied, it will query `par("bg")` only when
  `length(dev.list())>0`, otherwise it uses `"white"` by default. Note
  that calling [`par()`](https://rdrr.io/r/graphics/par.html) always
  opens a new graphics device if one is not already open, even though
  the `"bg"` value is stored somewhere and should not require a new
  device. The R docs do describe this behavior, for what it’s worth.
- [`imageByColors()`](https://jmw86069.github.io/jamba/reference/imageByColors.md)
  now accepts argument `cellnote` as an atomic vector, which is coerced
  to a matrix with same dimensions as input matrix `x`. This change
  makes it easier to use numeric formatting functions, which sometimes
  take matrix input and provide vector output.
- [`applyCLrange()`](https://jmw86069.github.io/jamba/reference/applyCLrange.md)
  new argument `fixup` which is passed to
  [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md)
  which in turn passes it to
  [`colorspace::hex()`](https://colorspace.R-Forge.R-project.org/reference/hex.html)
  when converting colors outside the color gamut. These functions will
  soon be replaced with calls to `farver`.
- [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md)
  and
  [`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md)
  now have argument option `model="hcl"` that uses farver, and its color
  model `"hcl"`, which is equivalent to using
  `colorspace::hex(...,fixup=TRUE)`, except faster, and with better
  upside for other color manipulations. For now, the color model will
  not use `"hcl"` until I can work out better default values in
  [`colorjam::rainbowJam()`](https://jmw86069.github.io/colorjam/reference/rainbowJam.html).

## jamba version 0.0.46.900

### Enhancements

- [`rbindList()`](https://jmw86069.github.io/jamba/reference/rbindList.md)
  has two optional arguments:

## jamba version 0.0.45.900

### new functions

- [`sdima()`](https://jmw86069.github.io/jamba/reference/sdim.md) and
  [`ssdima()`](https://jmw86069.github.io/jamba/reference/sdim.md) are
  equivalent to
  [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) and
  [`ssdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) except
  that they operate on `attributes(x)`. I somehow found myself
  constantly typing `sdim(attributes(x))` and finally had enough of
  that.
- [`printDebugI()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  is a shortcut to `printDebug(..., invert=TRUE)`, intended to be a
  quick way to use inverted colors, to colorize the background instead
  of the text. Note that
  [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  applies a color range restriction, to make sure the text contrasts
  with either light or dark background. When `invert=TRUE` the
  background color is not restricted, instead the foreground text uses
  [`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md)
  to determine a suitable contrasting color for the text and its
  corresponding background color.

### changes to existing functions

- [`imageByColors()`](https://jmw86069.github.io/jamba/reference/imageByColors.md)
  was refactored to handle grouped labels more cleanly. New argument
  `groupByColors=TRUE` decides whether to group cellnotes only when the
  label and underlying color are both identical – which is a change from
  previous behavior. Previous behavior grouped consecutive identical
  labels, regardless the underlying color, which is sometimes helpful,
  for example labeling a color gradient by the base color. Typically the
  cellnote and color would change together, but not always. The new
  default is consistent with the expected behavior.
- [`imageByColors()`](https://jmw86069.github.io/jamba/reference/imageByColors.md)
  new argument `groupBy` with one or both values `"row"` which groups
  cellnote values by row, and `"column"` which groups cellnote values by
  column. Useful to limit cellnote grouping so the values are not
  grouped inappropriately. This argument is mostly useful when
  `groupByColors=FALSE`.
- [`imageByColors()`](https://jmw86069.github.io/jamba/reference/imageByColors.md)
  new argument `adjustMargins=TRUE` will call
  [`adjustAxisLabelMargins()`](https://jmw86069.github.io/jamba/reference/adjustAxisLabelMargins.md)
  to adjust the margins to ensure the labels will fit in the plot device
  size.
- [`adjustAxisLabelMargins()`](https://jmw86069.github.io/jamba/reference/adjustAxisLabelMargins.md)
  was updated to handle `par("cex")` and `par("cex.axis")`
  appropriately.

## jamba version 0.0.44.900

### enhancements

- [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) and
  [`ssdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) now
  handle environment input, or list of environments. Previously worked
  for [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) but
  using random order returned by `names(x)` instead of proper order from
  `ls(envir=x)`. An environment is essentially treated as a list, and
  [`ssdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) will
  now properly traverse into the environment as if it were a list.
- [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) and
  [`ssdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) now
  handle `"List"` object classes and sub-classes from the `S4Vectors`
  Bioconductor package, but it only works when the `S4Vectors` package
  is previously loaded into the search path, otherwise the R object
  object system does not load the dependent package automatically. In
  that case, the `slotNames(x)` are used, which is somewhat less useful.
- [`imageDefault()`](https://jmw86069.github.io/jamba/reference/imageDefault.md)
  now properly honors the `par("bty")` setting, that is by not calling
  its own [`box()`](https://rdrr.io/r/graphics/box.html) to force
  drawing a box around the heatmap.

## jamba version 0.0.43.900

### bug fixes / enhancements

- [`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md)
  fixed issue with background color supplied in vector form, it was
  errantly calling
  [`fixYellow()`](https://jmw86069.github.io/jamba/reference/fixYellow.md)
  with the logical vector of whether to colorize, instead of the color
  vector itself. This package needs `"testthat"`.

## jamba version 0.0.42.900

### bug fixes / enhancements

- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  now handles nested lists, calling
  [`jamba::unnestList()`](https://jmw86069.github.io/jamba/reference/unnestList.md)
  which flattens nested lists to one layer. This change allows printing
  a list `printDebug(c("one", "two "), c("three", "four"))` or
  `l <- list(c("one", "two "), c("three", "four"));printDebug(l);`.
- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  now uses
  [`color2gradient()`](https://jmw86069.github.io/jamba/reference/color2gradient.md)
  to create alternating light-dark shadings, used when a single color is
  defined for a multi-item vector concatenated by `sep=","`. The
  gradient was more reliable (so far) than
  [`makeColorDarker()`](https://jmw86069.github.io/jamba/reference/makeColorDarker.md)
  because ANSI output is limited, as are the colors allowed for contrast
  with light or dark background.
- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  new argument `htmlOut=TRUE` will output character string containing
  text colorized using HTML using the format
  `<span style="color:red;background=white">text</span>`. Intended
  whenever the colorized text would not otherwise be interpreted and
  colorized in a web browser context. It calls
  [`make_html_styles()`](https://jmw86069.github.io/jamba/reference/make_html_styles.md).
- [`make_html_styles()`](https://jmw86069.github.io/jamba/reference/make_html_styles.md)
  is a new function, which takes a vector of text, a vector of
  foreground colors, a vector of background colors, and returns a
  character string with HTML which colorizes the text. This function is
  intended for Rmarkdown or web page HTML output.
- [`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md) was
  updated to handle arguments with vectors containing negative numbers.
  The negative `-` sign for example from `-3` is being returned by
  [`formals()`](https://rdrr.io/r/base/formals.html) as
  `call(\\`-\\`, 3)`, which stores the numeric value separate from the
  negative sign. This seems like a change, which therefore broke how the
  colorization previously worked. For now the function works, but
  somehow lost the ability to colorize numeric vectors by value. Will
  leave for future.

## jamba version 0.0.41.900

### bug fixes

- [`pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md)
  properly handles leading blanks, it previously only correctly handled
  blank values in the second and subsequent columns. Caused by trying to
  simplify the R function when moving into the jamba package.

## jamba version 0.0.40.900

### changes

- [`plotPolygonDensity()`](https://jmw86069.github.io/jamba/reference/plotPolygonDensity.md)
  new argument `highlightPoints` allows highlighting points in the
  distribution, which is currently displayed as histogram bars on top of
  the histogram and density polygon fill. Also slight change to how the
  polygon density is sized, it uses the median ratio of the height of
  the density curve to the histogram bars at the center of each bar.

## jamba version 0.0.39.900

### changes

- [`drawLabels()`](https://jmw86069.github.io/jamba/reference/drawLabels.md)
  first argument is `txt` which is the label to be displayed, a small
  convenience change.
- `breaksByVectors()` examples were updated to fix a typo when calling
  [`adjustAxisLabelMargins()`](https://jmw86069.github.io/jamba/reference/adjustAxisLabelMargins.md).
- Added substantial content to the vignette, including moving multiple
  images to `docs/articles` for pkgdown.

### possible breaking changes

- [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md)
  default `doSort=TRUE` is changed to `doSort=FALSE`. New functions:
  [`cPasteS()`](https://jmw86069.github.io/jamba/reference/cPaste.md) is
  intended for sorted values;
  [`cPasteU()`](https://jmw86069.github.io/jamba/reference/cPaste.md) is
  an alias for
  [`cPasteUnique()`](https://jmw86069.github.io/jamba/reference/cPaste.md)
  and is intended for unique values;
  [`cPasteSU()`](https://jmw86069.github.io/jamba/reference/cPaste.md)
  is intended for sorted unique values. They all call
  [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md) but
  with convenience defaults. It is recommended to use
  [`cPasteS()`](https://jmw86069.github.io/jamba/reference/cPaste.md) to
  replace sorted
  [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md) in
  cases where output is expected to be sorted. The change was made now
  while impact is limited mostly to other Jam packages and could be
  mitigated.

### new function

- [`groupedAxis()`](https://jmw86069.github.io/jamba/reference/groupedAxis.md)
  draws grouped axis labels, a small convenience function that extends
  `breaksByVectors()`.

### bug fixes

- Fixed regression in
  [`provigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md)
  when input is not character, the use of
  [`make.unique()`](https://rdrr.io/r/base/make.unique.html) requires
  explicit conversion to character type first.

## jamba version 0.0.38.900

### bug fixes

- [`provigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md)
  was enhanced to handle duplicated input entries, which previously
  returned only the unique entries, but now correctly returns all
  entries including duplicates, in the proper order. The optional list
  output also makes each list element unique. New argument `value`
  allows returning index positions, which is equivalent to
  [`proigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md).
  Lastly, the `ignore.case` argument is now properly honored, in order
  to allow case-sensitive matching.
- [`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md)
  will not try to use `rstudio::getThemeInfo()` if that package is
  installed, and if the function API exists, in order to determine
  whether Rstudio is currently using a dark theme.
- [`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md)
  has a new optional argument `bg_style` which allows defining the
  foreground `style` and background `bg_style` in one step. When
  `bg_style` is supplied, the Crange and Lrange arguments are ignored.
  Fixed a bug when rendering in Rstudio, where an ANSI foreground color
  of white does not get properly reset, causing all subsequent
  foreground colors to be white despite clear ANSI codes for different
  colors. The workaround is to render white as slightly off-white
  (greyscale 254 instead of 255) which restores correct output.
- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  was enhanced to use the `bg_style` argument of
  [`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md).
  New argument `invert` which will switch foreground and background
  colors. Fixed some follow-up issues with handling empty strings.
- [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md)
  fixed issue with `cPaste(NULL)` and `cPaste(list(NULL))` which
  previously caused an error, now returns `""` consistent with output
  from `cPaste(list(NULL, letters[1:2]))` where NULL entries produce
  `""`.
- [`fixYellow()`](https://jmw86069.github.io/jamba/reference/fixYellow.md)
  new argument `fixup` passed to
  [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md)
  which fixes out of gamut colors to scale within viewable range.
- [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
  was not properly saving rownames with argument `keepRownames=TRUE`,
  this issue was resolved.

## jamba version 0.0.37.900

### bug fixes

- Fixed a typo in
  [`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md)
  where the default Lrange was `getOption("jam.Crange")` instead of
  `getOption("jam.Lrange")`. The affect is limited to debugging output
  when printed on a dark background.
- [`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md) was
  updated to handle `object_size` above 2.1e9 (2 Gigabytes), it
  previously converted to integer which does not handle values that
  large, but now converts to numeric.

### new functions

- [`log2signed()`](https://jmw86069.github.io/jamba/reference/log2signed.md)
  performs log2 transformation of the magnitude of the numeric values,
  while preserving the directionality. It effectively performs
  `log1p(abs(x))*sign(x)` except that by default it uses log base 2.
- [`exp2signed()`](https://jmw86069.github.io/jamba/reference/exp2signed.md)
  is the reciprocal to
  [`log2signed()`](https://jmw86069.github.io/jamba/reference/log2signed.md),
  it exponentiates the magnitude of the numeric values, while preserving
  the directionality.

### changes

- Cleaned up unnecessary verbose output from
  [`minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md).

## jamba version 0.0.36.900

### bug fix

- Fixed embarrassing type in R package dependencies, “S4Vector” instead
  of “S4Vectors”.

## jamba version 0.0.35.900

### changes to existing functions

- [`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md)
  default hclCutoff was changed from 73 to 60, in order to use dark text
  more often for the “in between” colors.
- [`makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md)
  now uses [`duplicated()`](https://rdrr.io/r/base/duplicated.html) to
  detect duplicates prior to assigning names, which is substantially
  faster for long vectors where not all entries are duplicated, when
  compared to applying `table(x)` to the entire vector. For large
  vectors with mostly duplicated values, the speed is the same as
  before. Note this change also affects
  [`nameVector()`](https://jmw86069.github.io/jamba/reference/nameVector.md).
- [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) now
  handles a special case of S4 object where data is stored in one slot
  with slotName `".Data"`, for example `"limma::MArrayLM-class"` uses
  this format. When the length of `names(x)` matches the length of
  `slot(x, ".Data")` then those names are applied.
- [`sclass()`](https://jmw86069.github.io/jamba/reference/sclass.md) now
  properly detects S4 objects, and specifically recognizes `data.frame`
  as a list even though it secretly has `slotNames` like an S4 object,
  while not being an S4 object. (Is it?) Matrix-like objects return the
  class of each column for consistency, even though all columns have
  identical class. This function also handles the same special case S4
  scenario when it has only one slotName `".Data"`, as described above
  for [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md).

### bug fixes and changes to mixedOrder() family of functions

- [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md)
  did not sort properly when any entry contained only `"-"` dashes, this
  problem has been fixed. Previously “-” was potentially used to
  indicate negative numbers and therefore not treated as a blank value,
  inadvertently becoming `NA` and disrupting the ordering. Now strings
  `"-"` and `"---"` are considered blanks.

- [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md)
  now uses the input string as a final tiebreaker, so blank strings like
  `"--"` and `"-"` will also be sorted `c("-", "--")` in the output
  instead of returning in the same order as the input.

- [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md),
  several regular expressions were updated to cover edge cases, and will
  soon be wrapped into a series of `"testthat"` unit tests, long
  overdue. Also, verbose output was slightly updated. Specifically:

  - blanks are properly ordered at the front when `blanksFirst=TRUE`,
    and at the end when `blanksFirst=FALSE`.
  - NA values are also properly ordered at the front and end with
    `NAlast=FALSE` and `NAlast=TRUE` respectively. The NA order has
    priority over blanks, so NA will always be completely first or
    completely last.
  - Infinite values, when `keepInfinite=TRUE` will be ordered at the
    end, but before blanks, then before NA values, if those values are
    also positioned at the end. Also, when `keepNegative=TRUE` then
    `"-Inf"` will be position at the beginning, but after NA then after
    blanks, if those values are also positioned at the beginning.
  - `keepNegative=TRUE` also enables recognition of scientific notation,
    but that regular expression wrongly allowed decimal exponentials,
    which is not valid (e.g. “1.23e1.2” is not valid.) now the
    exponential only includes non-decimal numeric values,
    e.g. `"-1.23e2.2"` is effectively considered `"-1.23e2"` and `"2"`.

- [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md),
  [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)
  and
  [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
  are all affected by the changes to
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md).

- [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)
  was updated to enable correct behavior when `sortByName=TRUE`, which
  only works when all vectors in the input list are named.

- [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)
  will accept nested lists, and would have used the ultra-cool
  [`utils::relist()`](https://rdrr.io/r/utils/relist.html) function,
  except that function does not allow re-ordering the vector names
  within the nested list. So a new function
  [`relist_named()`](https://jmw86069.github.io/jamba/reference/relist_named.md)
  was added to jamba. It also works with `sortByName=TRUE` to sort each
  vector by its names, by only if all vectors are named.

### new functions

- [`relist_named()`](https://jmw86069.github.io/jamba/reference/relist_named.md)
  is a small modification of the
  [`utils::relist()`](https://rdrr.io/r/utils/relist.html) generic
  function (actually mostly just the `utils:::relist.default()`). This
  function splits a vector into a list matching the structure of a
  skeleton list, except that the names of each vector in the output list
  will match the input vector `x` and not the names from the vectors in
  the skeleton list.

## jamba version 0.0.34.900

### changes to existing functions

- [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md) now
  properly handles a list with factor and non-factor types, previously
  [`base::unlist()`](https://rdrr.io/r/base/unlist.html) coerced the
  mixed list to character which changed factor values to integers when
  non-factor elements were present. Now `keepFactors=TRUE` will preserve
  factor levels when sorting with `doSort=TRUE`.

## jamba version 0.0.33.900

### new functions

- [`gsubOrdered()`](https://jmw86069.github.io/jamba/reference/gsubOrdered.md)
  applies [`base::gsub()`](https://rdrr.io/r/base/grep.html) to
  character or factor input, and returns a factor output, retaining the
  order of levels based upon the input.

## jamba version 0.0.32.900

### changes to existing functions

- [`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md)
  changed the default `sortVars=FALSE` which prints function arguments
  in the order they are defined.

### new functions

- [`mergeAllXY()`](https://jmw86069.github.io/jamba/reference/mergeAllXY.md)
  merges multiple data.frames, keeping all rows and columns.
- [`unnestList()`](https://jmw86069.github.io/jamba/reference/unnestList.md)
  flattens a nested list-of-lists into a simple, flat list.

## jamba version 0.0.31.900

### bug fixes

- [`makeColorDarker()`](https://jmw86069.github.io/jamba/reference/makeColorDarker.md)
  fixed issue with repeated colors and darkFactors, processed using
  unique combination of color,darkFactor,sFactor to improve efficiency
  with large vectors, but was not properly handling the unique vectors.

## jamba version 0.0.30.900

### changes

- revised help text for
  [`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md)
- [`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md)
  uses
  [`colorspace::sRGB()`](https://colorspace.R-Forge.R-project.org/reference/sRGB.html)
  instead of
  [`colorspace::RGB()`](https://colorspace.R-Forge.R-project.org/reference/RGB.html)
  to match the convention that
  [`colorspace::hex2RGB()`](https://colorspace.R-Forge.R-project.org/reference/hex2RGB.html)
  actually uses *sRGB* and not *RGB* as the function name (and all
  common sense in the realm of how to name a function) implies.
- [`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md)
  and
  [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md)
  allow selecting from `polarLUV` or `polarLAB` color models, both of
  which use HCL values to generate colors.
- [`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md)
  no longer forces the output vector to have names, however it retains
  names if provided in the input.
- [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md)
  will use `polarLUV` or `polarLAB` as input without converting to an
  intermediate color space, which makes output slightly less lossy than
  converting to and from sRGB.

### new functions

- [`ucfirst()`](https://jmw86069.github.io/jamba/reference/ucfirst.md)
  is a simple function to uppercase the first letter in a word or
  phrase.
- [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)
  will sort a list of vectors using
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  alphanumeric sort, and fairly efficiently too! It sorts the unlisted
  data then splits it back to a list.
- [`drawLabels()`](https://jmw86069.github.io/jamba/reference/drawLabels.md)
  and
  [`coordPresets()`](https://jmw86069.github.io/jamba/reference/coordPresets.md)
  were moved from the `jamma` package. The
  [`drawLabels()`](https://jmw86069.github.io/jamba/reference/drawLabels.md)
  can be used to add text labels to a plot, with convenient coordinate
  presets handled by
  [`coordPresets()`](https://jmw86069.github.io/jamba/reference/coordPresets.md)
  which include things like “topleft”, “center”, or “right”. It
  automatically adjusts the text position to stay inside the plot border
  in those cases.

## jamba version 0.0.29.900

### new functions

- [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
  is a wrapper for the `openxlsx` package, intended to encapsulate a
  large number of default options for certain “known” column data types,
  like P-values, log2 fold changes, and integer values.
- [`applyXlsxCategoricalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxCategoricalFormat.md)
  and
  [`applyXlsxConditionalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxConditionalFormat.md)
  to apply colors to an existing Excel xlsx file worksheet. These
  functions are called by
  [`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)
  but can also be called manually to customize an existing Excel file.

### changes

- Added “openxlsx” to Suggests.

## jamba version 0.0.28.900

### bug fixes

- [`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md) was
  updated to handle empty units, useful for numeric values especially
  when kiloSize=1000. Numbers are also trimmed through
  [`format()`](https://rdrr.io/r/base/format.html) independently so the
  number of digits is not propagated through all values.

## jamba version 0.0.27.900

### enhancements

- [`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md)
  has been updated to handle alpha transparency, semi-transparent colors
  are blended with the background color, then compared to the threshold.

## jamba version 0.0.26.900

### new functions

- [`formatInt()`](https://jmw86069.github.io/jamba/reference/formatInt.md)
  is a quick wrapper function to format numeric values as integers, by
  default rounding decimal values and using big.mark=“,”, which can be
  overridden as needed.
- [`list2df()`](https://jmw86069.github.io/jamba/reference/list2df.md)
  to convert a list of vectors into a tall data.frame.

## jamba version 0.0.25.900

### bug fixes

- `warpAroundZero() was not properly handling`baseline\`, the method now
  uses a more straightforward approach.

### enhancements

- The site pkgdown documentation was updated to include each function in
  one (or sometimes multiple) categories, to help in finding potentially
  useful functions.
- The README.Rmd file has been stripped of the examples, which were a
  bit out of date compared to the function docs, and were not intended
  to be their own vignette. As I write this, I realize “removed visual
  examples” is rarely a positive step.

## jamba version 0.0.24.900

### new functions

- [`pasteByRowOrdered()`](https://jmw86069.github.io/jamba/reference/pasteByRowOrdered.md)
  is an extension to
  [`pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md)
  which creates an ordered factor, where the individual column order is
  maintained, either using existing factor level ordering, or via
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  per column.

## jamba version 0.0.23.900

### new functions

- [`unalpha()`](https://jmw86069.github.io/jamba/reference/unalpha.md)
  simply removes the alpha transparency from an R hex color, which is
  required for some plotly functions for example.
- [`isTRUEV()`](https://jmw86069.github.io/jamba/reference/isTRUEV.md)
  and
  [`isFALSEV()`](https://jmw86069.github.io/jamba/reference/isFALSEV.md)
  are vectorized forms of
  [`isTRUE()`](https://rdrr.io/r/base/Logic.html) and
  [`isFALSE()`](https://rdrr.io/r/base/Logic.html). As they take vector
  input, the entire vector must be `logical` in order for any entry to
  be considered.

### changes to existing functions

- [`rgb2col()`](https://jmw86069.github.io/jamba/reference/rgb2col.md)
  was enhanced so the `alpha` argument conveys whether to return alpha
  transparency in the output string. It seemed to be a convenient place
  to control alpha since the conversion of color names to hex requires
  converting to RGB as an intermediate, then back to hex color.

## jamba version 0.0.22.900

### changes to existing functions

- [`minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md)
  and
  [`minorLogTicksAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md)
  had an argument renamed to `offset` instead of `labelValueOffset`.
  Better to change it now before it has wider use. Also, the defaults
  were changed to `logBase=2` and `displayBase=10` based upon the
  current most frequent usage. Best to set these values explicitly.
  Ideally, R graphical parameters should define the log base instead of
  TRUE/FALSE in `par("xlog")` and `par("ylog")`.
- [`normScale()`](https://jmw86069.github.io/jamba/reference/normScale.md)
  was modified so the `low` and `high` range values are properly honored
  even when supplying a single value for `x`. Now the behavior checks if
  `low` is equal `high` to determine whether to use the `low` and `high`
  values. This adjustment helps `applyCLranges()` restrict colors to
  allowed ranges without over-correcting single colors to the midpoint
  of the range.

### new functions

- Added function
  [`sqrtAxis()`](https://jmw86069.github.io/jamba/reference/sqrtAxis.md)
  which computes axis tick mark positions for data that has been square
  root transformed, using transformation `sqrt(abs(x))*sign(x)` in order
  to maintain positive and negative values. This function is called by
  [`plotPolygonDensity()`](https://jmw86069.github.io/jamba/reference/plotPolygonDensity.md)
  when using `xScale="sqrt"`.

### bug fixed

- Cleaned up an issue with
  [`getColorRamp()`](https://jmw86069.github.io/jamba/reference/getColorRamp.md)
  with RColorBrewer colors, where it previously would retrieve colors
  using a number higher then the actual number supplied by
  [`RColorBrewer::brewer.pal()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html),
  and which only issues a warning (which I had hidden.) The code now
  checks for the proper maximum number, and expands to the larger number
  using
  [`colorRampPalette()`](https://rdrr.io/r/grDevices/colorRamp.html) as
  needed.

## jamba version 0.0.21.900

### bug fixes

- Updated
  [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md) to
  handle lists with a single NA value, which failed previously because a
  single NA is considered class “logical” and not “character” as
  required by
  [`S4Vectors::unstrsplit()`](https://rdrr.io/pkg/S4Vectors/man/character-utils.html).
- Changed how `Crange` and `Lrange` values are used in
  [`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md),
  the new default will not overwrite the global `Crange` and `Lrange`
  settings.
- Changed how `Crange` and `Lrange` values are enforced by
  `applyCLranges()`. There is a new argument `CLmethod` which controls
  how C and L values are adjusted.

## jamba version 0.0.20.900

### new functions

- [`fillBlanks()`](https://jmw86069.github.io/jamba/reference/fillBlanks.md)
  is used to fill a vector that has missing values, and fills missing
  values with the most recent non-blank value. It is useful when
  importing Excel data where headings might be present in the first cell
  of a block of cells, followed by blanks.
- [`newestFile()`](https://jmw86069.github.io/jamba/reference/newestFile.md)
  takes a vector of files, and returns the most recently modified, using
  [`file.info()`](https://rdrr.io/r/base/file.info.html).

## jamba version 0.0.19.900

### new functions

- [`grepls()`](https://jmw86069.github.io/jamba/reference/grepls.md) a
  useful utility for search the active environment by object name. For
  example `grepls("statshits")` will find everything named “statshits”,
  or `grepls("farris")` will find everything with “farris” in the name”,
  even within packages, or other attached environments in the search
  space.
- [`warpRamp()`](https://jmw86069.github.io/jamba/reference/warpRamp.md)
  takes a vector of colors as from a color gradient, and warps the
  gradient. For divergent colors, the adjustmet is symmetric around the
  middle color; otherwise the adjustment is relative to the first color.
  Helpful for adjusting colors scales in heatmaps.

## jamba version 0.0.18.900

### bug fixes

- [`imageDefault()`](https://jmw86069.github.io/jamba/reference/imageDefault.md),
  [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md),
  [`smoothScatterJam()`](https://jmw86069.github.io/jamba/reference/smoothScatterJam.md)
  were updated to allow values lower than 200 for `nbin` which
  previously caused problems when `fixRasterRatio=TRUE` and
  `oldstyle=TRUE`.
- [`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md) by
  default returns `NULL` when given `NULL` input, unless `nullValue` is
  defined. This change fixed several warnings, and resolved
  inconsistencies with
  [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md)
  not handling NULL parameters properly, consequently affecting
  [`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md)
  ability to restrict the color chroma (C) and luminance (L) ranges,
  seen when
  [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  did not use scaled colors.
- Fixed bug in
  [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md)
  during detection of RGB values above 255, which sometimes happens
  during the
  [`colorspace::polarLUV()`](https://colorspace.R-Forge.R-project.org/reference/polarLUV.html)
  conversion from HCL to RGB.

### new functions

- [`rlengths()`](https://jmw86069.github.io/jamba/reference/rlengths.md)
  returns the recursive lengths of list elements, intended when a list
  may have multiple nested lists. It returns the top level of counts per
  list element by default, but with `doSum=FALSE` it returns the full
  structure with the length of each non-list element.

## jamba version 0.0.17.900

### new functions

- [`minorLogTicksAxis()`](https://jmw86069.github.io/jamba/reference/minorLogTicksAxis.md)
  and
  [`minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md)
  provide log-transformed axis labels, with the added benefit of
  handling `log(1+x)` transformations properly; or log2FoldChange data
  with symmetry around zero.
- [`renameColumn()`](https://jmw86069.github.io/jamba/reference/renameColumn.md)
  a basic function but which allows re-running the function without
  error.
- [`normScale()`](https://jmw86069.github.io/jamba/reference/normScale.md)
  scales a numeric vector into a fixed range, by default between 0
  and 1. If one value is given, a parameter `singletMethod` defines
  whether to use the minimum, maximum, or mean of the range.
- [`rowGroupMeans()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md)
  and
  [`rowRmMadOutliers()`](https://jmw86069.github.io/jamba/reference/rowRmMadOutliers.md)
  are used to compute per-row mean values, with grouped columns.
  Optional outlier detection is performed using a MAD factor cutoff, for
  example 5xMAD threshold means points 5 times the MAD for the group are
  considered outliers.
- [`plotPolygonDensity()`](https://jmw86069.github.io/jamba/reference/plotPolygonDensity.md)
  which is a wrapper around
  [`hist()`](https://rdrr.io/r/graphics/hist.html) and
  `plot(density(x))`, but with some added features like scaling the
  x-axis by log10 or sqrt; and multi-panel output when the input is a
  multi-column matrix.
- [`warpAroundZero()`](https://jmw86069.github.io/jamba/reference/warpAroundZero.md)
  takes a numeric vector, and warps the values with a log curvature,
  symmetric around zero. The intent is to create non-linear breaks when
  used in heatmaps with divergent color ramps.

### bug fixed

- [`ssdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) was
  updated again to try to handle more non-list object types, for example
  the “bga” class returned from made4::bga().

## jamba version 0.0.16.900

### bug fixes

- Fixed small issue with R docs that did not properly wrap the `%>%`
  inside `\code{\%>\%}` blocks, which allows the percent sign to be
  escaped and ignored.
- Fixed numerous incorrectly formatted `\code{\link{}}` sections which
  did not properly specify the package name. The issue apparently only
  causes errors during package install on systems using HTML as the
  preferred help format.

## jamba version 0.0.15.900

### bug fixes

- [`smoothScatterJam()`](https://jmw86069.github.io/jamba/reference/smoothScatterJam.md)
  and
  [`imageDefault()`](https://jmw86069.github.io/jamba/reference/imageDefault.md)
  were updated to fix edge cases where the image being rendered was
  expected to have integer axis units, but where the raster ratio had
  duplicated columns or rows, making the units imprecise.
- [`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)
  has default `projectName=NULL` instead of trying to pull from
  `projectName` from the `.GlobalEnv` environment, which would cause an
  error if not defined. Instead, if `NULL` then it checks if
  `projectName` exists, if so the value is used.

## jamba version 0.0.14.900

### bug fixes

- Fixed issue with
  [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) so it
  would properly traverse a non-vector and non-list object which may
  contain elements with length or dimension.

## jamba version 0.0.13.900

### enhancements

- [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md)
  was adjusted to use global options where available, and by default
  will update global options when not defined. This mechanism allows one
  to set specific Crange and Lrange values, and have them be re-used by
  functions affected by those values.
- [`ssdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) now
  handles S4 objects by iterating each slotName and calling
  [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md).
- [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) now
  properly handles detection of S4 classes.

### new functions

- [`fixYellowHue()`](https://jmw86069.github.io/jamba/reference/fixYellowHue.md)
  takes a matrix of HCL colors (as from
  [`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md))
  and adjusts colors between hue 80 and 90 so they appear less green.
  This change is a visual preference that the default yellow appears
  green when darkened, and simply makes the color appear less green.
- `fixYellowColor()` takes a vector of colors as a wrapper to
  [`fixYellowHue()`](https://jmw86069.github.io/jamba/reference/fixYellowHue.md).

## jamba version 0.0.12.900

The main change in this version was to reduce required R version to
3.0.0.

## jamba version 0.0.11.900

### new functions

- [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md) is
  an efficient method of pasting a list of vectors using a delimiter, to
  create a character vector. It uses
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md)
  to sort entries, and optionally applies uniqueness to the vectors.
- [`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md)
  takes a list and makes each vector in the list unique, using an
  efficient mechanism that applies uniqueness to the overall set of
  values at once.

### Suggests S4Vectors

- The Bioconductor package `S4Vectors` implements efficient
  `unstrsplit()` used by
  [`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md).
  However, S4Vectors therefore requires installing Bioconductor base
  packages, which might be a heavy installation requirement just for
  improved efficiency at this step. In future this one function may be
  reproduced here (with permission of author Dr. Pages) to reduce the
  dependency burden.

## jamba version 0.0.9.900

### enhancements

- [`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md) does
  a better job of handling functions with nested lists, and closes any
  open `[` with corresponding `]`.
- [`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md)
  properly honors `useColor=FALSE` to disable colorized text, and adds
  `colNULL` to assign a color for `NULL` values, by default grey.
- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md),
  [`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md)
  will apply a CL range, chroma (C) and luminance (L) values, which are
  defined in part by `lightMode` and corresponding function
  [`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md),
  and a helper function
  [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md)
  which sets sensible default values for light and dark backgrounds.
- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  has two optional parameters `Crange` and `Lrange` which define a
  chroma (C) range and luminance (L) range. For lighter background, a
  maximum luminance of 80 seems reasonable; for darker background
  colors, minimum luminance of 75 is effective. The only awkwardness is
  that yellow darkens to a greenish hue, which is not aesthetically
  pleasing, so because I even noticed it, I had to “fix” the issue by
  adjusting yellows to more gold before darkening, enabled with default
  argument `fixYellow=TRUE`.
- [`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md)
  calls
  [`applyCLrange()`](https://jmw86069.github.io/jamba/reference/applyCLrange.md)
  which fixes the chroma (C) and luminance (L) values, hopefully to
  sensible default values.

### New functions

- [`applyCLrange()`](https://jmw86069.github.io/jamba/reference/applyCLrange.md)
  takes a vector of colors, and fixes the chroma (C) and luminance (L)
  values to this range. If `Cgrey` is above zero, then colors with
  chroma (C) value below this threshold are kept unchanged, in order to
  prevent a purely grey (unsaturated) color from becoming colorized.
- [`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md)
  will query
  [`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md)
  and return `Crange` and `Lrange` default values.
- [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md)
  which simply converts the output of
  [`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md)
  back to a vector of R hex colors. It should not be lossy, meaning it
  should faithfully convert R colors to an HCL matrix, then back to the
  same R original hex colors. Alpha transparency is also maintained.
  Note that
  [`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md)
  performs differently than
  [`grDevices::hcl()`](https://rdrr.io/r/grDevices/hcl.html) which does
  not produce the same colors as `colorspace::polarLUV(H, C, L)` using
  the same values for `H`, `C`, and `L`. The reasons unclear, however
  the functions in `jamba` are internally consistent, using `colorspace`
  functions.

## jamba version 0.0.6.900

### enhancements

- jamba now uses some global options(), intended only for function
  parameters that affect visual output, and will not affect analysis
  processing. Examples:
  - `jam.adjustRgb` - numeric, sets the parameter “`adjustRgb`” in
    printDebug().
  - `jam.lightMode` - boolean, sets the lightMode, see below.
- [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  has a new boolean parameter “lightMode”, which is TRUE when the
  background color is light (e.g. white or light-grey.) In this case, it
  applies a ceiling to the brightness of all text colors to ensure none
  are too bright to be visible.
- [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
  now accepts character values for `byCols`, with optional prefix “-” to
  indicate decreasing sort. Alternatively, the parameter decreasing will
  still be used to reverse the sort, and is converted to a vector when
  byCols has multiple values. Note that the prefix “-” and decreasing
  are multiplied to combine them.
- [`smoothScatterJam()`](https://jmw86069.github.io/jamba/reference/smoothScatterJam.md)
  tests if postPlotHook is a function, in which case it runs
  `postPlotHook(...)` otherwise it simply evaluates `postPlotHook;`
- [`nullPlot()`](https://jmw86069.github.io/jamba/reference/nullPlot.md),
  [`usrBox()`](https://jmw86069.github.io/jamba/reference/usrBox.md),
  and
  [`imageDefault()`](https://jmw86069.github.io/jamba/reference/imageDefault.md)
  have a new boolen parameter “`add`” indicating whether to create a new
  plot or add to an existing plot device.
- [`rbindList()`](https://jmw86069.github.io/jamba/reference/rbindList.md)
  has a new boolean parameter `keepNA` indicating whether to keep NA
  values in the results, which ultimately causes NA to be converted to
  “NA”.
- [`makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md)
  has a new boolean parameter `keepNA` indicating whether to keep NA
  values in the results. When keepNA is TRUE, NA is converted to “NA”,
  otherwise NA entries are treated as “” prior to creating names.
- [`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md) changed
  the default value for parameter `rmNAnames` to `FALSE`, which was the
  more anticipated behavior. An input vector will not be shortened for
  non-NA values that have an NA name. Changing function parameter
  defaults will be an extremely rare occurrence in future.
- [`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md)
  which is a vectorized wrapper for
  [`crayon::make_style()`](http://r-lib.github.io/crayon/reference/make_style.md),
  gains some finer control over color chroma and luminance.
- [`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md) gets
  a larger refactoring, aimed at better display of list parameters, and
  lists of lists and other nested variations. It now does a better job
  of displaying the list and vector names appropriately. A new function
  [`handleArgsText()`](https://jmw86069.github.io/jamba/reference/handleArgsText.md)
  was split into a separate function, but is a rare non-exported
  function since it is currently only useful for
  [`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md).
- TODO.md was created to keep track of future enhancements and fixes.

### new functions

- `applyLceiling()` intended to restrict colors to a maximum luminance
  in HCL color space, mainly to support the new lightMode option
  intended for using
  [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
  with a light background.
- [`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md)
  intends to support the new lightMode option by doing its best to check
  situations to enable lightMode. It first checks options(“lightMode”).
  It then checks environment variables that suggest Rstudio is running,
  in which case it defaults to lightMode=TRUE, since the default Rstudio
  has a light background.
- [`sclass()`](https://jmw86069.github.io/jamba/reference/sclass.md),
  [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md), and
  [`ssdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) are
  intended to help handle list objects.
  - [`sclass()`](https://jmw86069.github.io/jamba/reference/sclass.md)
    returns the class of each list element.
  - [`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md)
    returns the dimensions (or lengths) or each list element.
  - [`ssdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) is a
    special case that returns the dimensions of a list of list of
    objects. In all cases, if the input object is an S4 object, it
    operates on `slotNames(x)`. Thus, calling `ssdim(x)` is helpful for
    S4 objects, since it returns the class and dimensions of each object
    inside the S4 object.
