# Extend kableExtra colorization of 'Rmarkdown' tables

Extend kableExtra colorization of 'Rmarkdown' tables

## Usage

``` r
kable_coloring(
  df,
  colorSub = NULL,
  background_as_tile = TRUE,
  color_cells = TRUE,
  row_color_by = NULL,
  sep = "_",
  border_left = "1px solid #DDDDDD",
  border_right = FALSE,
  extra_css = "white-space: nowrap;",
  format = "html",
  format.args = list(trim = TRUE, big.mark = ","),
  row.names = NA,
  align = NULL,
  return_type = c("kable", "data.frame"),
  verbose = FALSE,
  ...
)
```

## Arguments

- df:

  `data.frame` input. Note that `kable` input is not supported.

- colorSub:

  one of the following inputs:

  - `character` vector of R colors, whose names match entries in the
    `data.frame` which are given these assigned colors

  - `function` that takes column values as input, and returns a
    `character` vector with one color per value, using `NA` or 'NULL' to
    indicate `"transparent"`

  - `list` whose names match `colnames(df)`, where each entry contains
    either `character` or `function` option as described above. A
    `character` vector should be named by values expected in each
    column. A `function` should take column values as input, and return
    a `character` vector with same length of R colors.

- background_as_tile:

  `logical` default `TRUE`, whether the cell background color will
  appear as a rounded tile (`TRUE`) or a rectangle (`FALSE`). Either
  way, the color does not fill the entire whitespace of the table cell,
  but only around the text itself.

- color_cells:

  `logical` indicating whether to color individual cells, default
  `TRUE`. This may be `FALSE` when also applying `row_color_by`, so the
  entire row will be colorized.

- row_color_by:

  `character` vector with one or more `colnames`, indicating how to
  colorize entire rows of a table. When one column is defined, colors in
  `colorSub` are used as normal. When multiple columns are defined,
  values from each column are concatenated using `sep` delimiter. Then
  resulting values are compared with `colorSub`.

- sep:

  `character` delimiter used to combine values in multiple columns when
  `row_color_by` is supplied and contains multiple `colnames`. The
  delimited character strings are compared to `colorSub` to assign
  colors.

- border_left, border_right, extra_css:

  `character` values optionally passed to
  [`kableExtra::column_spec()`](https://rdrr.io/pkg/kableExtra/man/column_spec.html)
  as a convenient way to apply borders for each column (`border_left`,
  `border_right`) or enable or disable word-wrapping by column. Some
  helpful examples:

  - `border_left=FALSE`: disables left border

  - `border_left="1px solid #DDDDDD"`: light gray 1 pixel left border

  - `border_right=FALSE`: disables right border

  - `border_right="1px solid #DDDDDD"`: light gray 1 pixel right border

  - `extra_css=NULL`: disables word-wrap

  - `extra_css="whitespace: nowrap;"`: enables text word-wrap

  - when all options above contain only `FALSE` or 'NULL', then
    [`kableExtra::column_spec()`](https://rdrr.io/pkg/kableExtra/man/column_spec.html)
    is not applied.

- format:

  `character` passed to
  [`knitr::kable()`](https://rdrr.io/pkg/knitr/man/kable.html), default
  `"html"` which is the intended format for most scenarios. It can be
  set to 'NULL' to enable auto-detection of the format.

- format.args:

  `list` of arguments passed to
  [`base::format()`](https://rdrr.io/r/base/format.html) intended mainly
  for `numeric` columns.

- row.names:

  `logical` indicating whether to include `rownames(df)`. When
  `row.names=NA` the default is to display rownames if they are not
  'NULL' and not equal to `1:nrow(df)`.

- align:

  `character` passed to
  [`kableExtra::kable()`](https://rdrr.io/pkg/knitr/man/kable.html) to
  define alignment of each column.

- return_type:

  `character` string indicating the type of data to return.

  - `return_type="kable"`: (default) returns object with class
    `"kableExtra", "knitr_kable"` suitable for downstream processing.

  - `return_type="data.frame"`: returns a `data.frame` whose cells
    contain HTML markup with corresponding colors defined.

- verbose:

  boolean indicating whether to print verbose output.

- ...:

  additional arguments are passed to
  [`kableExtra::kable()`](https://rdrr.io/pkg/knitr/man/kable.html)
  which allows the usual customizations on the initial call.

## Value

object with class `c("kableExtra", "knitr_kable")` by default when
`return_type="kable"`, suitable to render inside an 'Rmarkdown' or HTML
context. Or returns `data.frame` when `return_type="data.frame"`.

## Details

This function extends the `kableExtra` package, and is only available
for use if the `kableExtra` package is installed. It is intended to
allow specific color assignment of elements in a data.frame, but
otherwise uses the `kableExtra` functions to apply those colors.

The use case is to provide colorized HTML output for 'Rmarkdown', it has
not been tested with other `format` output.

The argument `colorSub` accepts:

- `character` vector input where names should match column values

- `function` that accepts column values and returns a `character` vector
  of colors of equal length

- `list` input where names should match `colnames(df)`, and where each
  list element should contain either a `character` vector, or `function`
  as described above.

## See also

Other jam color functions:
[`alpha2col()`](https://jmw86069.github.io/jamba/reference/alpha2col.md),
[`applyCLrange()`](https://jmw86069.github.io/jamba/reference/applyCLrange.md),
[`col2alpha()`](https://jmw86069.github.io/jamba/reference/col2alpha.md),
[`col2hcl()`](https://jmw86069.github.io/jamba/reference/col2hcl.md),
[`col2hsl()`](https://jmw86069.github.io/jamba/reference/col2hsl.md),
[`col2hsv()`](https://jmw86069.github.io/jamba/reference/col2hsv.md),
[`color2gradient()`](https://jmw86069.github.io/jamba/reference/color2gradient.md),
[`fixYellow()`](https://jmw86069.github.io/jamba/reference/fixYellow.md),
[`fixYellowHue()`](https://jmw86069.github.io/jamba/reference/fixYellowHue.md),
[`getColorRamp()`](https://jmw86069.github.io/jamba/reference/getColorRamp.md),
[`hcl2col()`](https://jmw86069.github.io/jamba/reference/hcl2col.md),
[`hsl2col()`](https://jmw86069.github.io/jamba/reference/hsl2col.md),
[`hsv2col()`](https://jmw86069.github.io/jamba/reference/hsv2col.md),
[`isColor()`](https://jmw86069.github.io/jamba/reference/isColor.md),
[`makeColorDarker()`](https://jmw86069.github.io/jamba/reference/makeColorDarker.md),
[`rainbow2()`](https://jmw86069.github.io/jamba/reference/rainbow2.md),
[`rgb2col()`](https://jmw86069.github.io/jamba/reference/rgb2col.md),
[`setCLranges()`](https://jmw86069.github.io/jamba/reference/setCLranges.md),
[`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md),
[`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md),
[`unalpha()`](https://jmw86069.github.io/jamba/reference/unalpha.md),
[`warpRamp()`](https://jmw86069.github.io/jamba/reference/warpRamp.md)

Other jam practical functions:
[`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md),
[`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md),
[`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md),
[`check_pkg_installed()`](https://jmw86069.github.io/jamba/reference/check_pkg_installed.md),
[`colNum2excelName()`](https://jmw86069.github.io/jamba/reference/colNum2excelName.md),
[`color_dither()`](https://jmw86069.github.io/jamba/reference/color_dither.md),
[`exp2signed()`](https://jmw86069.github.io/jamba/reference/exp2signed.md),
[`getAxisLabel()`](https://jmw86069.github.io/jamba/reference/getAxisLabel.md),
[`isFALSEV()`](https://jmw86069.github.io/jamba/reference/isFALSEV.md),
[`isTRUEV()`](https://jmw86069.github.io/jamba/reference/isTRUEV.md),
[`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md),
[`lldf()`](https://jmw86069.github.io/jamba/reference/lldf.md),
[`log2signed()`](https://jmw86069.github.io/jamba/reference/log2signed.md),
[`middle()`](https://jmw86069.github.io/jamba/reference/middle.md),
[`minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md),
[`newestFile()`](https://jmw86069.github.io/jamba/reference/newestFile.md),
[`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md),
[`reload_qmd_cache()`](https://jmw86069.github.io/jamba/reference/reload_qmd_cache.md),
[`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md),
[`renameColumn()`](https://jmw86069.github.io/jamba/reference/renameColumn.md),
[`rmInfinite()`](https://jmw86069.github.io/jamba/reference/rmInfinite.md),
[`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md),
[`rmNAs()`](https://jmw86069.github.io/jamba/reference/rmNAs.md),
[`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md),
[`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)

## Examples

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
   nameVector(
      color2gradient("grey48", n=3, dex=10),
      rep("rep", 3),
      suffix=""),
   nameVector(
      color2gradient(n=3,
         c("goldenrod1", "indianred3", "royalblue3", "darkorchid4")),
      expt_df$Sample_ID))
kbl <- kable_coloring(
   expt_df,
   caption="Experiment design table showing categorical color assignment.",
   colorSub)
# Note that the HTML table is rendered in 'Rmarkdown', not pkgdown
kbl
#> <table class="table" style="margin-left: auto; margin-right: auto;">
#> <caption>Experiment design table showing categorical color assignment.</caption>
#>  <thead>
#>   <tr>
#>    <th style="text-align:left;"> Sample_ID </th>
#>    <th style="text-align:left;"> Treatment </th>
#>    <th style="text-align:left;"> Genotype </th>
#>    <th style="text-align:left;"> Rep </th>
#>   </tr>
#>  </thead>
#> <tbody>
#>   <tr>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 204, 76, 255) !important;">Vehicle_Wildtype_rep1</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: palegoldenrod !important;">Vehicle</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">Wildtype</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(192, 192, 192, 255) !important;">rep1</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(232, 175, 34, 255) !important;">Vehicle_Wildtype_rep2</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: palegoldenrod !important;">Vehicle</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">Wildtype</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(122, 122, 122, 255) !important;">rep2</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(208, 156, 24, 255) !important;">Vehicle_Wildtype_rep3</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: palegoldenrod !important;">Vehicle</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">Wildtype</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(71, 71, 71, 255) !important;">rep3</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(218, 118, 118, 255) !important;">Vehicle_Knockout_rep1</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: palegoldenrod !important;">Vehicle</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: firebrick !important;">Knockout</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(192, 192, 192, 255) !important;">rep1</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(205, 85, 85, 255) !important;">Vehicle_Knockout_rep2</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: palegoldenrod !important;">Vehicle</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: firebrick !important;">Knockout</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(122, 122, 122, 255) !important;">rep2</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(158, 49, 49, 255) !important;">Vehicle_Knockout_rep3</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: palegoldenrod !important;">Vehicle</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: firebrick !important;">Knockout</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(71, 71, 71, 255) !important;">rep3</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(100, 130, 219, 255) !important;">Dex_Wildtype_rep1</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: navy !important;">Dex</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">Wildtype</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(192, 192, 192, 255) !important;">rep1</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(58, 95, 205, 255) !important;">Dex_Wildtype_rep2</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: navy !important;">Dex</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">Wildtype</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(122, 122, 122, 255) !important;">rep2</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(31, 62, 154, 255) !important;">Dex_Wildtype_rep3</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: navy !important;">Dex</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;">Wildtype</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(71, 71, 71, 255) !important;">rep3</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(145, 81, 177, 255) !important;">Dex_Knockout_rep1</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: navy !important;">Dex</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: firebrick !important;">Knockout</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(192, 192, 192, 255) !important;">rep1</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(104, 34, 139, 255) !important;">Dex_Knockout_rep2</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: navy !important;">Dex</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: firebrick !important;">Knockout</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(122, 122, 122, 255) !important;">rep2</span> </td>
#>   </tr>
#>   <tr>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(73, 17, 102, 255) !important;">Dex_Knockout_rep3</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: navy !important;">Dex</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: firebrick !important;">Knockout</span> </td>
#>    <td style="text-align:left;border-left:1px solid #DDDDDD;white-space: nowrap;"> <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(71, 71, 71, 255) !important;">rep3</span> </td>
#>   </tr>
#> </tbody>
#> </table>

# return_type="data.frame" is a data.frame with HTML contents
kdf3 <- kable_coloring(
   return_type="data.frame",
   df=expt_df,
   colorSub=colorSub)
kdf3;
#>                                                                                                                                                                                                      Sample_ID
#> 1       <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(255, 204, 76, 255) !important;" >Vehicle_Wildtype_rep1</span>
#> 2       <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(232, 175, 34, 255) !important;" >Vehicle_Wildtype_rep2</span>
#> 3       <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(208, 156, 24, 255) !important;" >Vehicle_Wildtype_rep3</span>
#> 4      <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(218, 118, 118, 255) !important;" >Vehicle_Knockout_rep1</span>
#> 5  <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(205, 85, 85, 255) !important;" >Vehicle_Knockout_rep2</span>
#> 6  <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(158, 49, 49, 255) !important;" >Vehicle_Knockout_rep3</span>
#> 7    <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(100, 130, 219, 255) !important;" >Dex_Wildtype_rep1</span>
#> 8      <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(58, 95, 205, 255) !important;" >Dex_Wildtype_rep2</span>
#> 9      <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(31, 62, 154, 255) !important;" >Dex_Wildtype_rep3</span>
#> 10    <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(145, 81, 177, 255) !important;" >Dex_Knockout_rep1</span>
#> 11    <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(104, 34, 139, 255) !important;" >Dex_Knockout_rep2</span>
#> 12     <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(73, 17, 102, 255) !important;" >Dex_Knockout_rep3</span>
#>                                                                                                                                                                         Treatment
#> 1  <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: palegoldenrod !important;" >Vehicle</span>
#> 2  <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: palegoldenrod !important;" >Vehicle</span>
#> 3  <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: palegoldenrod !important;" >Vehicle</span>
#> 4  <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: palegoldenrod !important;" >Vehicle</span>
#> 5  <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: palegoldenrod !important;" >Vehicle</span>
#> 6  <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: palegoldenrod !important;" >Vehicle</span>
#> 7         <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: navy !important;" >Dex</span>
#> 8         <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: navy !important;" >Dex</span>
#> 9         <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: navy !important;" >Dex</span>
#> 10        <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: navy !important;" >Dex</span>
#> 11        <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: navy !important;" >Dex</span>
#> 12        <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: navy !important;" >Dex</span>
#>                                                                                                                                                                             Genotype
#> 1             <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;" >Wildtype</span>
#> 2             <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;" >Wildtype</span>
#> 3             <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;" >Wildtype</span>
#> 4  <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: firebrick !important;" >Knockout</span>
#> 5  <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: firebrick !important;" >Knockout</span>
#> 6  <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: firebrick !important;" >Knockout</span>
#> 7             <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;" >Wildtype</span>
#> 8             <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;" >Wildtype</span>
#> 9             <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: gold !important;" >Wildtype</span>
#> 10 <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: firebrick !important;" >Knockout</span>
#> 11 <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: firebrick !important;" >Knockout</span>
#> 12 <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: firebrick !important;" >Knockout</span>
#>                                                                                                                                                                                             Rep
#> 1        <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(192, 192, 192, 255) !important;" >rep1</span>
#> 2  <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(122, 122, 122, 255) !important;" >rep2</span>
#> 3     <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(71, 71, 71, 255) !important;" >rep3</span>
#> 4        <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(192, 192, 192, 255) !important;" >rep1</span>
#> 5  <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(122, 122, 122, 255) !important;" >rep2</span>
#> 6     <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(71, 71, 71, 255) !important;" >rep3</span>
#> 7        <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(192, 192, 192, 255) !important;" >rep1</span>
#> 8  <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(122, 122, 122, 255) !important;" >rep2</span>
#> 9     <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(71, 71, 71, 255) !important;" >rep3</span>
#> 10       <span style="     color: rgba(0, 0, 0, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(192, 192, 192, 255) !important;" >rep1</span>
#> 11 <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(122, 122, 122, 255) !important;" >rep2</span>
#> 12    <span style="     color: rgba(255, 255, 255, 255) !important;border-radius: 4px; padding-right: 4px; padding-left: 4px; background-color: rgba(71, 71, 71, 255) !important;" >rep3</span>
```
