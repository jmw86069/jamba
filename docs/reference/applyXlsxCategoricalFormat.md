# Add categorical colors to 'Excel' 'xlsx' worksheets

Add categorical colors to 'Excel' 'xlsx' worksheets

## Usage

``` r
applyXlsxCategoricalFormat(
  xlsxFile,
  sheet = 1,
  rowRange = NULL,
  colRange = NULL,
  colorSub = NULL,
  colorSubText = setTextContrastColor(colorSub),
  trimCatNames = TRUE,
  overwrite = TRUE,
  wrapText = FALSE,
  stack = TRUE,
  verbose = FALSE,
  ...
)
```

## Arguments

- xlsxFile:

  `character` filename to a file with ".xlsx" extension, or `Workbook`
  object defined in the `openxlsx` package. When `xlsxFile` is a
  `Workbook` the output is not saved to a file.

- sheet:

  `integer` index of the worksheet or worksheets.

- rowRange, colRange:

  `integer` vectors of rows and columns to apply categorical colors in
  the 'Excel' 'xlsx' worksheet, passed as
  `openxlsx::readWorkbook(..., rows=rowRange, cols=colRange)`. This step
  defines which columns are read from each workbook, however when
  `colorSub` is provided as a `list` whose names are intended to match
  [`colnames()`](https://rdrr.io/r/base/colnames.html), only matching
  colnames are processed.

- colorSub:

  one of the following types of input:

  - Named `character` vector of valid R colors, whose names correspond
    to values in worksheet cells.

  - Named `list` whose names correspond to colnames one or more
    workbooks in `sheet`. Each list element should be a `character`
    vector named by column values, or color `function` that takes column
    values and returns a `character` vector of colors for each value.

- colorSubText:

  optional `character` vector of colors, whose names correspond to
  values in the worksheet cells. In absence of a specific text color,
  [`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md)
  is used to define a contrasting text color to be visible on the
  colored background.

- trimCatNames:

  `logical` whether to trim whitespace and punctuation from `colorSub`
  and from 'Excel' cell fields before matching colors to 'Excel' values.

- overwrite:

  `logical` indicating whether new cell color styles should be forced
  overwrite of previous cell styles.

- wrapText:

  `logical` indicating whether to wrap text.

- stack:

  `logical` indicating whether new color rules should be applied above
  existing styles, many of whose styles may not affect the specific cell
  color, for example the font size and font name.

- verbose:

  `logical` indicating whether to print verbose output.

- ...:

  additional arguments are ignored.

## Value

`Workbook` object as defined by the `openxlsx` package is returned
invisibly with [`invisible()`](https://rdrr.io/r/base/invisible.html).
This `Workbook` can be used in argument `wb` to provide a speed boost
when saving multiple sheets to the same file.

## Details

This function is a convenient wrapper for applying categorical color
formatting to cell background colors, and applies a contrasting color to
the text in cells using
[`setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md).
It uses a named character vector of colors supplied as `colorSub` to
define cell background colors, and optionally `colorSubText` to define a
specific color for the cell text.

## See also

Other jam export functions:
[`applyXlsxConditionalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxConditionalFormat.md),
[`readOpenxlsx()`](https://jmw86069.github.io/jamba/reference/readOpenxlsx.md),
[`set_xlsx_colwidths()`](https://jmw86069.github.io/jamba/reference/set_xlsx_colwidths.md),
[`set_xlsx_rowheights()`](https://jmw86069.github.io/jamba/reference/set_xlsx_rowheights.md),
[`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)

## Examples

``` r
# write to tempfile for examples
if (check_pkg_installed("openxlsx")) {
   out_xlsx <- tempfile(pattern="writeOpenxlsx_", fileext=".xlsx")
   df <- data.frame(a=LETTERS[1:5], b=1:5);
   writeOpenxlsx(x=df,
      file=out_xlsx,
      sheetName="jamba_test");

   colorSub <- nameVector(
      rainbow2(5, s=c(0.8, 1), v=c(0.8, 1)),
      LETTERS[1:5]);
   applyXlsxCategoricalFormat(out_xlsx,
      sheet="jamba_test",
      colorSub=colorSub
   )
}
```
