# Set column widths in Xlsx files

Set column widths in Xlsx files

## Usage

``` r
set_xlsx_colwidths(
  xlsxFile,
  sheet = 1,
  cols = seq_along(widths),
  widths = 11,
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
  [`openxlsx::setColWidths()`](https://rdrr.io/pkg/openxlsx/man/setColWidths.html)
  indicating the worksheet to affect.

- cols:

  `integer vector` indicating the column numbers to affect.

- widths:

  `numeric vector` indicating the width of each column defined by
  `cols`.

- ...:

  additional arguments are passed to
  [`openxlsx::setColWidths()`](https://rdrr.io/pkg/openxlsx/man/setColWidths.html).

## Value

`Workbook` object as defined by the `openxlsx` package is returned
invisibly with [`invisible()`](https://rdrr.io/r/base/invisible.html).
This `Workbook` can be used in argument `wb` to provide a speed boost
when saving multiple sheets to the same file.

## Details

This function is a light wrapper to perform these steps from the very
useful `openxlsx` R package:

- [`openxlsx::loadWorkbook()`](https://rdrr.io/pkg/openxlsx/man/loadWorkbook.html)

- [`openxlsx::setColWidths()`](https://rdrr.io/pkg/openxlsx/man/setColWidths.html)

- [`openxlsx::saveWorkbook()`](https://rdrr.io/pkg/openxlsx/man/saveWorkbook.html)

## See also

Other jam export functions:
[`applyXlsxCategoricalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxCategoricalFormat.md),
[`applyXlsxConditionalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxConditionalFormat.md),
[`readOpenxlsx()`](https://jmw86069.github.io/jamba/reference/readOpenxlsx.md),
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

   ## By default, cols starts at column 1 and continues to length(widths)
   set_xlsx_colwidths(out_xlsx,
      sheet="jamba_test",
      widths=rep(20, ncol(df))
   )
}
```
