# Set row heights in Xlsx files

This function is a light wrapper to perform these steps from the very
useful `openxlsx` R package:

## Usage

``` r
set_xlsx_rowheights(
  xlsxFile,
  sheet = 1,
  rows = seq_along(heights) + 1,
  heights = 17,
  ...
)
```

## Arguments

- xlsxFile:

  `character` filename to a file with ".xlsx" extension, or `Workbook`
  object defined in the `openxlsx` package. When `xlsxFile` is a
  `Workbook` the output is not saved to a file.

- sheet:

  `integer` sheet number or `character` sheet name, passed to
  [`openxlsx::setRowHeights()`](https://rdrr.io/pkg/openxlsx/man/setRowHeights.html)
  indicating the worksheet to affect.

- rows:

  `integer vector` indicating the row numbers to affect.

- heights:

  `numeric vector` indicating the height of each column defined by
  `rows`.

- ...:

  additional arguments are passed to
  [`openxlsx::setRowHeights()`](https://rdrr.io/pkg/openxlsx/man/setRowHeights.html).

## Value

`Workbook` object as defined by the `openxlsx` package is returned
invisibly with [`invisible()`](https://rdrr.io/r/base/invisible.html).
This `Workbook` can be used in argument `wb` to provide a speed boost
when saving multiple sheets to the same file.

## Details

- [`openxlsx::loadWorkbook()`](https://rdrr.io/pkg/openxlsx/man/loadWorkbook.html)

- [`openxlsx::setRowHeights()`](https://rdrr.io/pkg/openxlsx/man/setRowHeights.html)

- [`openxlsx::saveWorkbook()`](https://rdrr.io/pkg/openxlsx/man/saveWorkbook.html)

Note that when only the argument `heights` is defined, the argument
`rows` will point to row 2 and lower, thus skipping the first (header)
row. Define `rows` specifically in order to affect the header row as
well.

## See also

Other jam export functions:
[`applyXlsxCategoricalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxCategoricalFormat.md),
[`applyXlsxConditionalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxConditionalFormat.md),
[`readOpenxlsx()`](https://jmw86069.github.io/jamba/reference/readOpenxlsx.md),
[`set_xlsx_colwidths()`](https://jmw86069.github.io/jamba/reference/set_xlsx_colwidths.md),
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

   ## by default, rows will start at row 2, skipping the header
   set_xlsx_rowheights(out_xlsx,
      sheet="jamba_test",
      heights=rep(17, nrow(df))
   )

   ## to include the header row
   set_xlsx_rowheights(out_xlsx,
      sheet="jamba_test",
      rows=seq_len(nrow(df)+1),
      heights=rep(17, nrow(df)+1)
   )
}
```
