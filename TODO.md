# TODO for jamba

## 13feb2025

* `asSize()` should call `object.size()` first when the input object
is not already `numeric`.

## 27jan2025

* `readOpenxlsx()`

   * Fix `rows`,`cols` arguments to subset rows and columns imported.
   * Handle multi-row headers formally:
   
      * Consider method to define specific header rows, instead of hoping
      they are detected correctly.
      * Define multi-row header delimiter, default `"_"`.
      * Fix/Improve handling of multi-row headers via `check_header=TRUE`.
      Currently doesn't work as intended.
   
         * "default": When multiple rows are "header", then delimit each row
         value into new column header.
         * "extend": As above, except when leading fields are `NA`, use
         most recent non-NA value as header, thus "extending" that header
         field to the next column. For example grouped columns, where the
         group name is only present in the first row,column of the group.

## 11dec2024

* `writeOpenxlsx()`

   * Consider `startRow`,`startCol` (`startColumn`) to define where
   to begin saving worksheet data.
   * Related, consider optional multi-row headers.
   Bonus points: merge cells for consecutive repeated headings,
   but (probably) not on the lowest/last header row.

* One more revamp to `rainbowJam()` to improve default colors.
Too much pink, too much "beige", and too few of the colors we love.

   * Design idea is to take `named_colors`, arrayed in rank order of hue
   (by degrees) and literally reassign color hue by the popularity of
   named colors. People don't name nearly as many greens as they name
   red-orange-yellow, which is partly because green hues are not easily
   resolved in detail.
   * (So green should not be nearly 1/3 the color wheel!)
   RGB wheel:
   `color_pie(jamba::hsv2col(rbind(h=seq(0, to=1, by=0.01), s=1, v=1)))`

* Update `named_colors`

## 09dec2024

* `setTextContrastColor()` - consider option `jam.hclCutoff` or
`jam.L_threshold` to control the breaking point for light/dark text.
* `hcl2col()` - fix `alpha` argument input, currently ignored.
* `shadowText()`

   * Apply more arguments in vectorized form: pos,adj

* `kable_coloring()`

   * Consider option to hide all borders (top/bottom/left/right).
   * Consider optional `text_color_sub` to control the text color,
   to support `venndir::textvenn()` output where some labels use fill color,
   other labels use font color.

## 24nov2024

* Fix error in `cPaste()` when `S4Vectors` is not installed.

## 01nov2024

* `jargs()` - consider word-wrapping when there are numerous items,
see: `jargs(multienrichjam::fixSetLabels, "words")`.

## 07oct2024

* Consider support for `Matrix` sparse matrix objects, `CsparseMatrix`

   * specifically for `rowGroupMeans()` which causes errors in `jamma`

## 13sep2024

* DONE. `kable_coloring()`

   * Fix bug where HTML tables show the HTML tags with escapes,
   something to do with `format="html"` not being included by default.

## 04sep2024

* `plotRidges()` - make it work when there is only one column.
* `plotPolygonDensity()` - make it reset the `par("mfrow")` after plotting.

## 13aug204

* `reload_rmarkdown_cache()`

   * The path should accept the base RMarkdown cache and find the proper
   subdirectory: `"./html"` or other relevant subdirectory names.
   * Currently stops with: `"No .rdata files found in directory."`

* Use `options` package to handle Jam-related package options

   * See: https://github.com/dgkf/options `options::define_option()`
   * Manages options, default values, ect.
   * Also creates documentation for `jamba::options` with all options,
   defaults, and descriptions.
   * Low overhead dependencies, only imports `utils`.
   * Add this stub:
   ```
   #' @eval options::as_roxygen_docs()
   NULL
   ```
   * Current options:
   
      * `shadowText()`: `jam.shadow`, jam.shadow.r, jam.shadow.n,
      jam.shadowColor, jam.alphaShadow, `jam.outline`, jam.alphaOutline
      * `printDebug()`: jam.fgDefault, jam.fgTime, jam.timeStamp,
      `jam.comment`, `jam.htmlOut`,
      * `jam.formatNumbers`, jam.trim, jam.digits, jam.nsmall,
      jam.big.mark, jam.small.mark
      * jam.doColor, jam.sep, jam.file, jam.append, jam.invert
      * color-related: `jam.lightMode`, jam.Crange, jam.Lrange, jam.Cgrey,
      jam.adjustRgb, `jam.model` (hcl, polarLUV, polarLAB)
      * jam.stopClasses

## 31jul2024

* `tcount()`

   * Pass `...` to `table(x)`, using `table(x, ...)` instead.
   The change will allow things like `useNA="ifany"`, or custom `exclude`.
   * Require input `x` is atomic `is.atomic(x)` to prevent weirdness
   with `list` or `data.frame` input.
   * Consider how to handle `table(x, y)` or `table(data.frame)`

## 09jul2024

* Consider `rsdim()` as scalable extension to `sdim()` and `ssdim()`

   * Goal is to permit `sdim()` logic on deeper nested objects than two levels.
   * Consider `sdim()` new argument `indent`, to help print nested `sdim()`.

* Consider new function: `remakeNames()`

   * Alternate to `makeNames()` that removes the version suffix, then
   reapplies a new suffix.
   * Optional `recursive=TRUE` to trim multiple suffix versions,
   for example `c("A_v1", "A_v1_v1", "A_v1_v2")` would eventually become
   `c("A", "A", "A")`.
   * Motivation is to prevent entries from being version-versioned,
   `c("A", "A_v1", "A_v1")` would become `c("A", "A_v1_v1", "A_v1_v2")`.
   A preferred option may be to consider the input as `c("A", "A", "A")`,
   then version those values to create: `c("A_v1", "A_v2", "A_v3")`.
   * The other motivation is to avoid having to "fix problems" within
   `makeNames()` when it could potentially create duplicates:
   `makeNames(c("A", "A", "A_v1"))` would output `c("A_v1", "A_v2", "A_v1")`,
   see below.

* Consider new function: `unmakeNames()`

   * Its purpose is to remove previous version suffix created by `makeNames()`.

* `makeNames()`

   * Resolve potential bug when the versioned name repeats a pre-existing
   value, for example `makeNames(c("A", "A", "A_v1"))` would create
   two entries `"NA_v1"`.
   * Three potential methods:
   
      1. "Reset versions":
      
         * Basically, any entry that has previously been versioned,
         causing newly versioned entries to fail, should be un-versioned,
         then re-versioned so it will succeed.
         * Added benefit that versions will be in order they appear
         in the output.
         * This process would be iterative:
         
            * All duplicated values would trigger `unmakeNames()` to remove
            the version suffix. All values with the same un-versioned value
            (which could use the input vector rather than actually
            un-versioning) would then be re-versioned together as a set.
            * This step could inadvertently create new duplicates, causing
            the process to occur again, and so on. It will eventually stop,
            given a finite input set.
         
         * `makeNames(c("A", "A", "A_v1"))` would change the `"A_v1"` entry
         to `"A"`, after which the output becomes `c("A_v1", "A_v2", "A_v3")`.
         * This option unfortunately changes entries, which can be confusing.
         For example, the entry which was originally `"A_v1"` is not the
         final entry called `"A_v1"`.
      
      2. "Append versions":
      
         * Basically if there are any duplicates after `makeNames()`,
         it calls `makeNames()` again iteratively until all entries
         are unique.
         * This option unfortunately appends multiple suffix versions.
         * It has one benefit that data might be traceable. This suffices
         `"_v1_v1"` and `"_v1_v2"` suggests that both entries were
         previously `"_v1"`.
         * It has slight risk of getting stuck in an iterative loop, with
         input data that already has multiple layers of versioning.
         * Example:
         `makeNames(c("A", "A", "A_v1"))` produces `c("A_v1", "A_v2", "A_v1")`,
         then `c("A_v1_v1", "A_v2", "A_v1_v2")`.

   3. "Keep existing versions":
   
      * More complicated logic. Keep existing versions as-is, and
      increment to the next available un-used version number.
      * If input data is already versioned, keep it as-is without change,
      unless there are duplicates among versioned values, in which case
      handle as usual.
      * Previously unversioned values which become duplicates should
      skip that version number. For example `makeNames(c("A", "A", "A_v2"))`
      should produce `c("A_v1", "A_v3", "A_v2")`.
      * The algorithm:
      Check for duplicates after versioning. If none, then end.
      If duplicates, by definition there must be only one from input,
      and only one created by versioning.
      Keep the entry that already existed, re-assign the other one.
      If there were two `c("A_v2", "A_v2")` input entries, they would have
      been versioned already to create `c("A_v2_v1", "A_v2_v2")`.
      The desired output is that multiple `"A"` entries should skip the
      `"_v2"` version if `"A_v2"` already exists, and continue with
      `"_v3"` and so on.
      The tricky part is that multiple versions could be assigned.
      So for 15 duplicates of `"A"` where `"A_v7"` and `"A_v11"` were
      already assigned, the method would know to create `15 + 2` suffix
      values, since there are 15 duplicated entries, but 2 existing entries.
      Then remove `"_v7"` and `"_v11"` because they already exist.
      It then assigns the remaining values in order.
      


## 20jun2024

* `setPrompt()`

   * consider option to include date and/or date-time stamp.
   Similar to a bash prompt style.
   Current default:
   `{project-name}-R-4.2.3_processid>`
   Suggested date option 1:
   `{project-name}-{20jun2024}-R-4.2.3_processid>`
   Suggested datetime option 1:
   `{project-name}-{20jun2024}-R-4.2.3_processid>`


   * Side notes for available date string options for `POSIXct`, `POSIXt`:
   
      * `%c` uses full time and date: `"Sun Jun  9 01:45:53 2024"`
      * `%H` is the hour in 24H scale with leading zero
      * `%I` is the hour in 12H scale with leading zero
      * `%p` indicates `"AM"` or `"PM"`
      * (Is there no representation for AM/PM?)
      * `%M` is the minute with leading zero
      * `%S` is the second with leading zero
      * `%T` is equivalent to `"%H:%M:%S"`
      * `%t` is replaced by a `tab`

* `getDate()`

   * Consider allowing `dateFormat` to be a jam option:
   `options("jam.dateFormat")`.
   Bonus point if `asDate()` can recognize a variety of common formats.

## 04jun2024

* DONE. `heatmap_column_order()`, `heatmap_row_order()`

   * Error seen with `HeatmapList` (after drawing a list of heatmaps)
   where there are multiple annotation heatmaps preceding the "main" heatmap.
   The preceding heatmaps do not have the row or column order, which causes
   it to fail.
   
      * The solution is to use the first entry with class `Heatmap` in the
      `HeatmapList` as `hm@ht_list`.
      * Also add new argument to select a specific heatmap.

* DONE. `mixedSortDF()`

   * Fix handling of `"mtime"`, `"POSIXct"`, `"POSIXt"`, `"Date"` classes
   which cannot be converted to `data.frame`.

* `sdim()` and `ssdim()`

   * When input is an `array` that contains `list` data, it returns the
   `lengths()` of the list elements, but it should return the dimensions
   of the enclosing `array`.

* Prepare more urgently for CRAN or Bioconductor submission.

## 27may2024

* `heatmap_column_group_labels()`

   * Fix bug when there is only one `column_split` values, which should
   enable drawing a box around the entire heatmap, with or without
   group lines and group labels. Error:
   `"Viewport 'expression_heatmap_body_1_7' was not found"`

## 22mar2024

* DONE. `writeOpenxlsx()`, `applyXlsxCategoricalFormat()`

   * It appears to be applying categorical colors to incorrect columns,
   as if they are out of sync.

* `setPrompt()` does not enable color when run inside RMarkdown.

## 24jan2024

* `reload_rmarkdown_cache()`

   * Accept cache directory even when there is a subdirectory
   such as `"project_cache/html/"`. It should also accept `"project_cache"`,
   and detect the presence of the subdirectory `"html"`.
   * Question: Are there other potential subdirectory names and
   is the subdirectory based upon the RMarkdown output format?
   See https://bookdown.org/yihui/rmarkdown-cookbook/cache-path.html,
   yes: `"html"`, `"latex"`, `"docx"`.

* `kable_coloring()`

   * Consider options similar to `writeOpenxlsx()`: highlightColumns,
   pvalueColumns, fcColumns, lfcColumns, numColumns, intColumns,
   in order of interest. The highlighting could be easy and useful.

## 10jan2024

* `heatmap_column_order()`

   * Current: When `HeatmapList` is supplied with multiple heatmaps,
   by default it returns only the column order for the first heatmap.
   * Future: It should give option to return column order for individual
   heatmaps, or across all heatmaps.

## 17nov2023

* `writeOpenxlsx()`

   * Consider adding optional multi-row headers
   * Consider adding ability to save table into an existing workbook
   starting at certain row,column position.
   
      * This feature could also be used to enable multi-row headers, and
      multi-column row annotations, for example saving a `SummarizedExperiment`
      or `matrix` object that contains rownames, and separate row annotations.
      

## 24oct2023

* DONE. `printDebug()`

   * DONE. debug issue where `htmlOut=TRUE` inserts multiple newlines `<br/>`
   * DONE. add convenience function: `printDebugHtml()` for use in RMarkdown,
   which enables `htmlOut=TRUE` and `comment=FALSE` by default.

## 03oct2023

* DELAYED: consider migrating `printDebug()` to use `cli::cli_alert_info()`

   * **Downside:** It does not print HTML output inside RMarkdown, instead
   falls back to uncolorized output. Oh well, color is the primary motivation.
   It also does not use fancy bullets in HTML, another cool feature lost.
   * Could be useful to employ the pattern of time/date stamp, and color-coding
   * potential benefit is that output could use R recommended methods
   
      * E.g. messaging, warnings, errors, etc.
      * Added features: inline markup; bullets; icons; word wrap;
      * Therefore output could be silenced using standard approaches.
      * It offers some improved formatting: bullets, icons, emojis, etc.
      (but not in HTML output.)
      * unclear if colorization is readily configurable, or if better to
      use `cli` patterns
      * To print a message:
      ```R
      save_file <- "output_file.txt";
      cli::cli_alert_info("Saved to: '{.field {save_file}}'.")
      cli::cli_alert("Saved to: '{.field {save_file}}'.")
      {options("warn"=FALSE)
      cli::cli_alert_warning("Saved to: '{.field {save_file}}'.")}
      cli::cli_alert_success("Saved to: '{.field {save_file}}'.")
      ```

## 28sep2023

* `writeOpenxlsx()`

   * consider adding `pctColumns`,`pctRule`,`pctColors` for percentage values
   * Consider updates when `startRow` is not 1, to write data into
   an existing workbook sheet? Driving use case is to organize multiple small
   `data.frame` into the same worksheet. It might therefore need a header row?

## 27aug2023

* `setPrompt()`

   * Debug why the prompt is not defined in color when called inside RMarkdown.

## 22aug2023

* `sdim()`

   * Consider option to print object size beside each row. This option
   would incur a performance hit, but could be useful for understanding
   which object components consume the most memory. Data in general is
   getting larger, and it could be helpful when trimming down unnecessary
   duplication of data in object `list` context for example.
   * Consider some way to indicate when an object is `S4`, which means
   its names are `slotNames()` and not just `names()`. Unclear how it
   would work in `ssdim()` where multiple `data.frame` results are intended
   to be stackable by columns, and would require the same `colnames()`.
   Find somewhere to indicate whether `slotNames()` or `names()` are shown.
   * Add more methods to recognize `names(x)`.
   * Find simple test cases that can test S3 or S4 objects.

## 17aug2023

* `printDebug()`

   * DONE. argument `comment` should take optional `character` string
   to use as prefix.
   * consider calling `cli::format_message` for tidyverse-compliant
   R messages and warnings.

## 10aug2023

* DONE. `applyXlsxCategoricalFormat()` and `writeOpenxlsx()`

   * DONE. argument `colorSub` should accept `list` input, similar to
   `kable_coloring()`. The `list` should be named by `colnames(x)`
   and accepts either a named `character` vector of colors so the
   names match values in the column, or a `function` that takes
   column values and returns a `character` vector of colors for
   each value in the column.

## 08aug2023

* `makeColorDarker()`

   * consider new argument `dex` "darkness expansion factor" similar
   to its use in `color2gradient()`
   * consider new argument `satex` "saturation expansion factor"

## 11jul2023

* DONE: `grepls()`

   * not properly applying regular expressions to search package functions,
   for example `library(multienrichjam);jamba::grepls("^enrich")` returns
   no results, but `jamba::grepls("enrich")` does return results.

* DONE: migrate `multienrichjam::gsubs()` into jamba.

   * Minor: Deprecate `gsubs()` in multienrichjam, such that it calls
   `jamba::gsubs()` when jamba is sufficient version.


## 05jul2023

* DONE: `plotSmoothScatter()`

   * when `asp` is defined, honor this setting by also defining
   axis ranges appropriately, not cropping points outside the range
   when an axis range is increased to accommodate the aspect ratio.
   One workaround is to use `nullPlot(asp=asp, doBoxes=FALSE)`,
   then use `par("usr")` to define actual `xlim`, `ylim` values,
   the `plotSmoothScatter(..., add=TRUE)` to inherit those options.

* `drawLabels()`

   * enhance the labels to allow `adj` to define text justification,
   so for example text can be left-aligned, right-aligned, or centered.
   Currently all text is centered, by defining the midpoint of the label
   itself, the using `adj=c(0.5, 0.5)`.
   In some cases it would be nice to have left-aligned text.
   For this change, text width would be calculated, then the x coordinate
   would be moved to the left side of the box, with buffer equal to half
   the difference in box width and text width.

## 12jun2023

* `printDebug()`, `printDebugI()`

   * Atomic elements with empty names `(NA, "", "^[\t ]+$")` (whitespace)
   should be printed using the element value and not its name.
   Currently, names are printed when they exist, but no attention
   is given to whether the names are empty.
   Unclear whether the expected/correct behavior would be to print all
   values, or a mixture of non-empty-names/values-instead-of-empty-names.
   For now, go with the second option, printing names when non-empty,
   and element values otherwise.
   * Consider argument `names_and_values=TRUE` which would optionally
   display names above values as used with `print()`. In this case,
   the names would be printed inverted (or when empty, the name would
   not be visible), and values would be printed using the same color
   but not inverted.

* RMarkdown use with colorized text output: `printDebug()`, `printDebugI()`.

   * Investigate how `pkgdown::build_article()` is able to produce
   RMarkdown/HTML output that includes colored text from `printDebug()`,
   but `knitr::render_markdown()` only captures color when using
   block option `results='asis'`.
   * Default yaml options include: `pkgdown: as_is: true`, so maybe it
   is preferred to keep `results='asis'`?

## 23mar2023

* `kable_coloring()`

   * DONE. Enable passing `sample_color_list` to colorize columns
   * DONE. Allow coloring `numeric` columns, perhaps through `sample_color_list`?
   Using `function` is more effective than `colorSub` for numeric values.
   * DONE. (By virtue of calling `platjam::design2colors()`.)
   Automate colorizing numeric columns by value, for example
   `platjam::df_to_numcolors()` except invisible. Maybe an optional
   argument enabling/disabling this feature, but enabled by default?
   * It should apply color to `row.names` when they are displayed.
   (Taken from 10jan2023 TODO item.)
   * Use bold font by default for row names.
   * Consider handling `matrix` input by using a color gradient to
   colorize `numeric` values.

## 22mar2023

* `imageByColors()`

   * `adjustMargins=TRUE` does not reset margins after plotting.
   It may be desirable, so other features can be added, but causes
   problems when running a series of plots.

## 27feb2023

* consider new color function: `is_color_ramp()`

   * simple function that performs the same test in `getColorRamp()`
   just to validate that an input `character` string is a possible color
   ramp.
   * potential argument to require the `character` string to be a specific
   named color ramp, and not a single color which could be converted
   into a color ramp.

## 06feb2023

* `mixedSorts()` optimization

   * DONE: when `honorFactor=FALSE` all class `"factor"` should be considered
   `"character"`, minimizing effects of handling `factor` and `character`
   data independently.
   * Handle other edge cases with mixed class, and nested or simple list.

* `reload_rmarkdown_cache()` should check subdirectory "./html/" if initial reload fails.
For some reason some RMarkdown output imposes an additional subdirectory "html" for html output.
For that matter, this function may need to check subdirectories and by default use the first
one found (without a dot prefix like ".git").

## 04feb2023

* run some performance profiling of core functions:

   * `mixedSort()` and related functions
   * `cPaste()` and related functions

## 10jan2023

* `kable_coloring()`

   * Categorical colors are assigned with `colorSub`, but should also
   be compatible with `color_list` output from `platjam::design2colors()`
   (soon to be moved into `colorjam`).
   * It should apply color to `row.names` when they are displayed.

## 28nov2022

* `jargs()`

   * it fails when functions follow the troubled paradigm for generics, e.g.:

      ```R
      some_func <- function(object, ...){
         .local <- function(object, arg1, arg2, arg3);
         .local(object, ...)
      }
      ```
   
   * Design idea: Figure out a way to "sniff" out these arguments.
   * Eval the function body, look for function named `.local`
   * It will fail for functions that call `UseMethod("mean")`, which
   requires the object class in order to search generics using the form
   `function.class1()`,  where `class1` is the first `"character"`
   value from `class(object)`. Optionally, `jargs_internal()` could
   have argument `class` or `object` to guide it to the appropriate
   method based upon the `class` or `class(object)`.
   * Driving example: `ComplexHeatmap::draw()`
   
      * can be found with `names(findMethods("draw"))`, for class `"Heatmap"`,
      `"HeatmapAnnotation"`, and several others.
      * `args(findMethods("draw")$Heatmap)` will show `function(object, ...)`
      * however the function body defines `.local` as a `function`


## 14nov2022

* `jargs()` is janky for some functions, for example arguments that
begin with parentheses. Other non-standard formatting has previously
caused problems, but examples like embedded functions appear to work fine.

   * Example that works: `jamses::heatmap_se()` argument
   `cluster_rows=function(x, ...){ amap::hcluster(x, ...) }`.
   Actually, this one fails to insert a comma between internal function
   arguments. Sigh.
   * Example that fails: `multienrichjam::layout_with_qfr()` argument
   `repulse.rad=(igraph::vcount(g)^repulse)`
   * Design idea: write a test routine that calls `jamba::jargs()` on
   all functions in a package, captures text output, then searches
   for any argument that spans multiple lines. Then print a visual summary
   to check for potential problems.

## 19oct2022

* `readOpenxlsx()` throws an error when column headers do not align
with subsequent data. Insert filler column headers.

   * Easiest workaround is change default to `check_header=FALSE`
   * The `check_header=TRUE` logic should change, the intent is to
   detect when one row of additional header is above column headers,
   usually seen with lower ncol, however sometimes real data has
   fewer ncol because the last column(s) contain empty cells.

## 14oct2022

* `renameColumn()` should accept `integer` values for `from` to indicate
column indexes. The driving use case is a matrix that lacks colnames,
`renameColumn(x, from=1:4, to=letters[1:4])`. Internally convert
`integer` or `numeric` values in `from` to `colnames(x)` or when
`length(colnames(x)) == 0` assign `colnames(x) <- seq_len(ncol(x))`,,
then convert `from <- as.character(from)`.


## 04oct2022

* `jargs()`

   * Option for multiple columns, for those functions with many arguments.
   Requires some estimate of the number of console columns, or user-defined.

* `jamsplom()`

   * new custom function that I still use often outside the Jam package
   context, useful enough to be added to jamba for convenience.
   It's as useful as `plotPolygoDensity()`, `plotSmoothScatter()`,
   `plotRidges()`, somewhat combined.

## 27sep2022

* `mixedSortDF()` does not work with time class such as `"POSIXct"`,
although `mixedSort()` does work. Coersion with `as.numeric()` should
suffice as a workaround.

## 01sep2022

* `readOpenxlsx()`

   * add option not to include colnames, equivalent of `colNames=FALSE`.
   This option will skip the step that loads colnames separately.
   * debug argument `rows`

## 26jul2022

* `adjustAxisLabelMargins()`

   * It should operate much like `par()`, by returning the `par()`
   values that were changed, so they can be changed back.
   * Should be an option so it defines `on.exit(par(opar))` to
   revert the margin values that it re-defines during its operation.

## 20jul2022

* `kable_coloring()` chokes when `colorSub` uses color names,
they just need to be converted to hex format beforehand.
* `cell_fun_label()`

   * arguments `prefix` and `suffix` are indexed by the list `m` and
   not by `show` which is confusing. Consider applying `prefix` and `suffix`
   in order to each entry in `show`.

* `cPasteU()` and `uniques()` appears slow in some situations

   * Apparently for small length vectors <1000, `lapply(x, unique)` is
   much faster than BioC methods with `unique(CharacterList(x))`
   * `unique(FactorList(x))` is extremely slow for large vectors
   * unclear whether `cPaste()` should ever handle `factor` without
   converting to `character` - all data should be `character` upfront,
   which may speed all downstream steps.

* `mixedSorts()` does not handle mixed `list` with `character` and `factor`

   * Easiest option that works*: when input is all the same `class`
   proceed with the optimization step currently used,
   otherwise revert to `lapply(x, mixedSort)` which is markedly slower,
   but more consistent with expected behavior.

* `mixedSort()` and `mixedSorts()` are inconsistent handling `factor`

   * `mixedSort()` should have option to honor `factor` order, consistent
   with `mixedSorts()`
   * `mixedSorts()` should handle mix of `character` and `factor` and return
   a `list` with the same classes as the input `list`.
   * suggest new argument `honorFactor=FALSE` default for `mixedSort()` to
   keep previous behavior without affecting dependencies
   * suggest new argument `honorFactor=TRUE` default for `mixedSorts()` to
   keep previous behavior without affecting dependencies

## 18jul2022

* COMPLETE: `printDebug()` updates

   * Situation: In Rmarkdown, when the R block uses `results='asis'`,
   see [RMarkdown Guide#generate-multiple-tables-from-a-for-loop](https://bookdown.org/yihui/rmarkdown-cookbook/kable.html#generate-multiple-tables-from-a-for-loop)).
   The output from `printDebug()` should be configurable for that format,
   maybe with some global option, so that all internal functions
   that call `printDebug()` will be affected the same way without having
   to pass an option through each child function in a process.

## 07jul2022

* `rmNA()` should work properly on data.frame and matrix objects,
returning the same object back, with the relevant values replaced
with `naValue` as relevant.

## 17jun2022

* add `log2fold_axis()` convenience function
* `plotSmoothScatter()` argument `transFactor` is not used by default,
the argument `transformation` should by default use `transFactor`.

## 25may2022

* Check `cell_fun_label`:

   * COMPLETE: It is possible to increase speed by skipping blank cells? (Yes.)


## 23may2022

* Allow list of colors/color functions as input to `writeOpenxlsx()`.

   * `platjam::design2colors()` soon to be migrated into `colorjam::design2colors()`
   * `design2colors()` outputs a `list` of named color vectors, and
   for `numeric` columns it returns color `function` defined by
   `circlize::colorRamp2()`. Not sure how well a custom color `function`
   will work in Excel, other than pre-defining cell color for each
   individual cell, which seems sub-optimal.
   * The main benefit would be that a color `list` can be matched to
   `colnames(x)`, and applied only to columns known to benefit from
   color assignment. Also, colors can be uniquely assigned by each
   column.

## 22may2022

* COMPLETE: `drawLabels()` option to force at least panel width,
intended to mimic ggplot facet labels.

## 05may2022

* COMPLETE. Consider new function specifically for MAD outlier point
filtering, instead of hiding this feature inside `rowGroupMeans()`.

   * I am so dumb, there is a function `rowRmMadOutliers()` for this purpose,
   however this function is not grouped, and is instead only per row.
   * COMPLETE. New function `rowGroupRmOutliers()`.

* Consider moving functions into this package which are currently
being used in multiple Jam packages:

   * `update_function_params()` - jamma multienrichjam
   * `update_list_elements()` - jamma multienrichjam (use `utils::modifyList()`)
   * `find_colname()` - jamma, multienrichjam

## 19apr2022

* Consider steps required to publish jamba on CRAN. (yike!)

   * Might need to add tests for every function (yike!)
   * Might need to remove some non-essential functions: `"jamrandom"`, `"jamitall"`?

* `printDebug()` and `printDebugI()` changes:

   * consider detecting "state" when run inside RMarkdown, although
   currently seems to require the R block options include `results='asis'`
   to enable color, which also requires `htmlOut=TRUE`.
   * `htmlOut=TRUE` does not de-colorize before applying the delimiter;
   also by default should remove comment prefix.

* consider moving all color-related functions into colorjam.

   * `hcl2col()`, `col2hcl()`, `rgb2col()`, `hsv2col()`, `col2hsv()`
   * check cross-jam-package dependencies, e.g. `jamba::rgb2col()` is in
   `splicejam`, `colorjam`, and a few R scripts.

* `writeOpenxlsx()` and related functions

   * Enable `startRow` to write data starting on row 2 or row 3 for example.
   * Add example syntax for saving multi-sheet data to file with one
   write operation, avoiding read/write for each worksheet.

* Convert `imageToColors()` to `ComplexHeatmap::Heatmap()`

   * or create a variation that uses ComplexHeatmap
   * requires function to label cells in the center of contiguous range of cells
   with same color and label


## 02dec2021

* COMPLETE: additional functions to convert to and from abbreviated
size `asSize()`


## 29nov2021

* COMPLETE: `mixedSortDF()` appears to have issues sorting data.frame columns with
type "integer64". It appears not to sort these columns at all.
* COMPLETE: `readOpenxlsx()` is throwing errors in edge cases, needs debugging.
* COMPLETE: `readOpenxlsx()` detect when there is one or more header lines that
are not column headers, for example when there is an overall title in the
first row or rows of each worksheet. It can be detected either by counting
`ncol()` for each of the first 5 rows individually, and by comparing column
classes for each row loaded individually.


## 22nov2021

* Consider adding version to `check_pkg_installed()` for additional
constraints. The idea is to check if the package is installed, and
optionally if the package version is at least `>= x.y.z`, likely
using `utils::compareVersion()`.
* COMPLETE: Update `getColorRamp()` to recognize any color ramps supplied by
`colorjam` if that package is installed: `jam_linear` and `jam_divergent`.


## 04nov2021

`mixedSortDF()` currently allows applying reverse sort by using 
colname prefix "-". For numeric columns it is sometimes useful to
sort the magnitude instead of the absolute value.

   * suggest new prefix "*" or "~" which may appear with or without "-".
   * when "~-" or "-~" appear as prefix, data is sorted by decreasing
   absolute value.
   * when only "~" appears as a prefix, data is sorted by increasing
   absolute value.
   * at some point sort columns could use non-standard evaluation, which
   could allow fancy math or logic to be applied here. Not yet.


### 05oct2021

Simple utility function to load a common set of JAM packages

* "omics" - loads jamba, jamma, colorjam, jamsession, jamses
* "regions" - as above plus GenomicRanges
* "rnaseq" - as above plus splicejam
* "peaks" - "omics", "regions", and slicejam
* "covhm" - "omics" and platjam


### 10aug2021

* COMPLETE: `writeOpenxlsx()` can be painfully slow with somewhat large
Excel sheets, roughly 20,000 rows and 11 columns.

   * The slow step is probably either `applyXlsxConditionalFormat()`,
   or `applyXlsxCategoricalFormat()`. 
   * Whichever step is slowest, it needs to become much faster.
   * Note: Slowness was resolved by converting functions to use
   Workbook instead of saving/loading/updating/saving.


### 25jul2021

* `geom_shadowText()` - essentially drop-in replacement for `geom_text()`
except that it uses `shadowText()` logic in ggplot2 context. There is
a similar package "shadowtext" by Dr.Guangchuang Yu, however that package
does not offer the features implemented here:

   * define the transparency of shadow or outline
   * define the color of each shadow
   * order rendering of shadows with each label and in vectorized context

### Functions to update

* DONE: `shadowText()` - consider re-ordering the rendering so that
the shadow for each text label is placed before the next label.
* DONE: `checkLightmode()` along with `printDebug()`, `makeStyles()`,
and `setPrompt()` collectively do not update light mode as
intended when used inside RStudio. The issue was with `setCLranges()`.

   * Also, once set, light mode is not as easily changed as intended.

* `colorRampPaletteJam()` - new function as a drop-in replacement
to `colorRampPalette()`; this alternative calls `blend_colors()`
between each pair of colors in order to create better intermediate
colors in the gradient. Examples: red-blue which should produce
purple intermediate; yellow-blue which should produce green
intermediate.
* `colorjam::blend_colors()` currently purple-gold and
purple-yellow create a green intermediate color, these
cases should go through red/brown. It requires either manual
correction and/or requires that purple-to-yellow shortest hue
angular path goes through red.

   * Consider using a slightly improved red-yellow-blue color hue wheel,
   with better delineation of the blue-purple-red transitions. The hope
   is that this color hue wheel might make purple-red-yellow the preferred
   (shorter) angular path between purple-to-yellow.
   * Consider manual correction inside `blend_colors()`
   for cases where colors are roughly 180 hue degrees different,
   instead of leaving it to chance.

* `colorjam::rainbowJam()` should have better mechanism to define
`preset` for custom color hue wheels, for example `"custom"` or
some option that uses the `h1` and `h2` hue conversion already
defined if it has been defined.


### Vignettes for common small use cases

* color manipulation

   * creating and adjusting color gradients
   
      * `getColorRamp()`
      * `showColors()`
   
   * creating categorical colors, using them to make gradients
   
      * `colorjam::rainbowJam()`
   
* log-transformation, sqrt-transformation and plotting appropriate axes
   
   * `log2signed()`, `exp2signed()`
   * `minorLogTicksAxis()`, `sqrtAxis()`
   * log-transformed P-values


### Usability

* FIXED: `plotSmoothScatter()` ignores `xlab` and `ylab`.
* `plotSmoothScatter()` using `bandwidthN` is confusing, it overrides `bwpi`.
* `getColorRamp()` value `defaultBaseColor="grey95"` is annoying to me,
change to `"grey98"` or `"grey99"`


### implement testthis unit testing


### new functions

* `splomSmooth()` 

   * all-versus-all scatter plot matrix using equivalent
   logic as used in `plotSmoothScatter()`, but using
   `lattice::splom()`. It is much more efficient than
   equivalent base and ggplot2 plot methods.
   * add option to "hide" combinations of panels, when
   those combinations are not relevant or appropriate
   to the visualization.
   * option to specify the specific color gradient in each
   cell, as opposed to combining the categorical color
   from each input column.
   * make options for bandwidth and visual bins more intuitive


### Bugs 28aug2020

* FIXED: `printDebug()` throws a warning about fixYellow:

> Warning message:
> In if (fixYellow) { :
>   the condition has length > 1 and only the first element will be used


### Bugs 29jul2020

* `writeOpenxlsx()` highlightColumns appears to prevent conditional
formatting on the same columns.
* `provigrep()` checks for names and assigns only if missing, it does
not correct duplicate names if they are already present.

### enhancements 29jul2020

* `tcount2()` - wrapper for `tcount(..., minCount=2)`

### migrate functions from other packages

* `find_colname()`
* `deconcat_df()`


### Enhance writeOpenxlsx() 08may2020

* `writeOpenxlsx()` option to supply a header which is displayed in
the top row of the Excel worksheet, with its own style. For example
the header may be a title "Data following normalization and limma analysis"
with dark blue background, white text, font size 18.

#### Nice to have

* `applyXlsxConditionalFormat()` should in theory color the text
with `setTextContrastColor()` to use white text on dark background
colors. Not sure if Excel allows it, or if it requires manually
setting the color in each cell.
* `writeOpenxlsx()` option to apply conditional formatting
to numeric columns using each column numeric range, not fixed range.
Other option is to supply a list of rules for `numRule`. Design idea:
When `numRule=NULL` or `numRule=NA` then create rule based upon
the numeric range (min, mean, max). Also, `numRule` can be a `list`,
for example `numRule=list(c(0,100,1000), NA, c(0,1,2))`. This rule
would apply `c(0,100,1000)` to the first column in `numColumns`,
then would apply `NA` to the second column (thus auto-scaled),
then would apply `c(0,1,2)` to the third column.

### Optimizations 29apr2020

* FIXED: `cPaste()` is not fully efficient when the input does not
require going through steps `unlist()` then `split()`. In these
cases the data should be left as `CharacterList` if possible,
to use `S4Vectors::unstrsplit()` directly. Conditions that
require `unlist()` then `split()`:

   * `na.rm=TRUE` and the presence of `NA` values
   * `doSort=TRUE` and any list element with multiple values

### Bug 02oct2019

* `plotSmoothScatter()` throws an error
`"is.numeric(xlim) is not TRUE"`
when plotting data with class `"difftime"`.

### General enhancements

* Add message during package install suggesting the "crayon" package be
installed if not already installed.
* Investigate the interplay between Mac/Linux TERM,
locale with LC_ALL, special characters like <ce> (theta),
and R help docs.

### Bug fixes / Enhancements to existing functions

1. `printDebug()` probably needs a specific delimiter between
lists. Currently `collapse` is accepted via `...` but separates
every entry.

### new plotSmoothScatterG with grouped colors

1. `plotSmoothScatterG()` a new function which would create a
smooth scatter plot like `plotSmoothScatter()` except that it will
also allow coloring points by group. The previous closest estimate
was the ability to overlay contours of different colors, however
the transparency of each layer is not effective, the last color
layer drawn becomes the dominant color.
This function essentially creates a layer for each color, then
blends them into one collective layer. Each layer should probably
be a `weight` applied to each layer of colors, in order to
avoid blending the paleness become dominant. The point color will
be determined by the weighted color blend, the intensity of the color
as a gradient starting from background color will be determined by
the sum of the weights, relative to the rest of the plot.

Required secondary functions:

* Weighted color blending function. Must blend yellow and blue to
make green, not grey (my own requirement). See `"jonclayden/shades"`,
or `colorspace::mixcolor()` though it only works with 2 colors.
* Weighted gradient color assignment, such that given any color,
and a weight from 0 to 1, returns a single color representing the
color at that position along the gradient from background to
foreground color.

Commentary: Does any one color space blend `red + yellow = orange`
and `blue + yellow = green`? Maybe `polarLUV`. Others either blend
`blue + yellow` into `grey` or `purple` ... makes zero sense (to me).

2. `printDebug()` option for HTML or Rmarkdown-friendly output.
Note this feature seems to work when producing HTML output from Rmarkdown.

3. `jargs()` is choking on certain argument formats:

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

* optional recursive operation, in the event of a nested list of data.frames,
it should recurse through the list, calling rbindList() on each list element
and progressive build up one resulting data.frame or matrix.
* optional ability to handle different colnames; alternative is to review
`data.table::rbindlist()` as potential replacement.

### imageByColors

* new argument `cellnoteColor` to define specific cellnote text color,
currently uses `setTextContrastColor()` but should be able to define custom
colors. (Setup for print colorized text data.frame using similar input.)
* optional boolean parameter to transpose the image, i.e. t(x) and
   if applicable t(cellnote).
* optionally draw boxes around grouped labels, a visual indicator of
   groups of cells sharing one label. Care should be taken not to enable
   this functionality with a large table containing no grouped labels, or
   even with any scenario resulting in "too many boxes".

### color brightness and saturation handling

* helper functions like darken(), lighten(), saturate(),
   desaturate(). Note colorspace::desaturate() completely removes all color
   saturation (chroma), and conflicts with this function naming scheme.
   Maybe: `jam_desaturate()`, `jam_darken()`, `jam_brighten()`, `jam_saturate()`.
* new function `subsetColors()` which internally creates a data.frame with
   hex, RGB, HSV, and HCL values, which can then be used to subset an input
   set of colors. Bonus points for accepting different color classes at
   input, and returning the same color class at output.

