# Import one or more data.frame from 'Excel' 'xlsx' format

Import one or more data.frame from 'Excel' 'xlsx' format

## Usage

``` r
readOpenxlsx(
  xlsx,
  sheet = NULL,
  startRow = 1,
  startCol = 1,
  rows = NULL,
  cols = NULL,
  check.names = FALSE,
  check_header = FALSE,
  check_header_n = 10,
  verbose = FALSE,
  ...
)
```

## Arguments

- xlsx:

  `character` path to an 'Excel' file in `xlsx` format, compatible with
  [`openxlsx::read.xlsx()`](https://rdrr.io/pkg/openxlsx/man/read.xlsx.html).

- sheet:

  one of 'NULL', `character`, or `integer` vector, where: `sheet=NULL`
  will import every sheet; `character` is a vector of sheet names; and
  `integer` is a vector of sheet index values. The sheet names are
  determined with
  [`openxlsx::getSheetNames()`](https://rdrr.io/pkg/openxlsx/man/getSheetNames.html).

- startRow:

  `integer` indicating the row number to start importing each `sheet`.

  - Note `startRow` can be a vector with length `length(sheet)`, to
    specify the `startRow` for each `sheet`.

  - Note `startRow` is ignored when `rows` is defined for the same
    sheet, to minimize confusion about using both togetheer.

- startCol:

  `integer` indicating the first column number to retain after importing
  each `sheet`.

  - Note `startCol` can be a vector with length `length(sheet)`, to
    specify the `startCol` for each `sheet`.

  - Note `startCol` is ignored when `cols` is defined for the same
    sheet, to minimize confusion about using both togetheer.

- rows:

  `integer` vector indicating specific rows to import for each `sheet`.

  - To specify different `rows` for each `sheet`, supply `rows` as a
    `list` of `integer` vectors.

  - Note that when `rows` is defined for a sheet, it will be used and
    `startRow` will be ignored for that same sheet.

- cols:

  `integer` vector indicating specific column numbers to import for each
  `sheet`.

  - To specify different `cols` for each `sheet`, supply `cols` as a
    `list` of `integer` vectors.

  - Note that when `cols` is defined for a sheet, it will be used and
    `startCol` will be ignored for that same sheet.

- check.names:

  `logical` indicating whether to call
  [`make.names()`](https://rdrr.io/r/base/make.names.html) on the
  `colnames` of each `data.frame`.

  - Note that
    [`openxlsx::read.xlsx()`](https://rdrr.io/pkg/openxlsx/man/read.xlsx.html)
    does not honor `check.names=FALSE`, so a workaround is applied which
    loads a single line without column headers, in order to obtain the
    same data without mangling column headers. If this process fails,
    another workaround is to use `startRow=2` (one higher than previous)
    and `colNames=FALSE`.

- check_header:

  `logical` indicating whether to test for presence of header rows,
  which may be multi-line column headers. When `check_header=TRUE`, this
  method simply tests for the presence of rows that have `ncol`
  different than the remaining rows of data in the given sheet. When
  header rows are detected, the values are assigned to column `dimnames`
  of the `data.frame`.

- check_header_n:

  `integer` number of rows to test for header rows, only used when
  `check_header=TRUE`. This step is intended when the top row(s) contain
  fewer columns with headers, above actual column headers, for example
  the first row `c("Sample", "", "", "Lane", "")`, and the second row
  `c("Name", "Type", "Label", "Name", "Type")`. In this case the desired
  output is
  `"Sample_Name","Sample_Type","Sample_Label","Lane_Name","Lane_Type")`.
  This option default is `FALSE` due to the number of exceptions seen in
  real data.

- verbose:

  `logical` indicating whether to print verbose output.

- ...:

  additional arguments are passed to
  [`openxlsx::read.xlsx()`](https://rdrr.io/pkg/openxlsx/man/read.xlsx.html).

## Value

`list` of `data.frame` objects, one per sheet in `xlsx`.

## Details

This function is equivalent to
[`openxlsx::read.xlsx()`](https://rdrr.io/pkg/openxlsx/man/read.xlsx.html)
with a few minor additions:

1.  It returns a `list` of `data.frame` objects, one per `sheet`.

2.  It properly reads the `colnames` with `check.names=FALSE`.

By default this function returns every `sheet` for a given `xlsx` file.

Some useful details:

- Empty columns are not skipped during loading, which means a worksheet
  whose data starts at column 3 will be returned with two empty columns,
  followed by data from that worksheet. Similarly, any empty columns in
  the middle of the data in that worksheet will be included in the
  output.

- When both `startRow` and `rows` are applied, `rows` takes priority and
  will be used instead of `startRows`. In fact `startRows` will be
  defined `startRows <- min(rows)` for each relevant worksheet. However,
  for each worksheet either argument can be 'NULL'.

## See also

Other jam export functions:
[`applyXlsxCategoricalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxCategoricalFormat.md),
[`applyXlsxConditionalFormat()`](https://jmw86069.github.io/jamba/reference/applyXlsxConditionalFormat.md),
[`set_xlsx_colwidths()`](https://jmw86069.github.io/jamba/reference/set_xlsx_colwidths.md),
[`set_xlsx_rowheights()`](https://jmw86069.github.io/jamba/reference/set_xlsx_rowheights.md),
[`writeOpenxlsx()`](https://jmw86069.github.io/jamba/reference/writeOpenxlsx.md)

## Examples

``` r
# set up a test data.frame
set.seed(123);
lfc <- -3:3 + stats::rnorm(7)/3;
colorSub <- nameVector(
   rainbow2(7),
   LETTERS[1:7])
df <- data.frame(name=LETTERS[1:7],
   int=round(4^(1:7)),
   num=(1:7)*4-2 + stats::rnorm(7),
   fold=2^abs(lfc)*sign(lfc),
   lfc=lfc,
   pvalue=10^(-1:-7 + stats::rnorm(7)),
   hit=sample(c(-1,0,0,1,1), replace=TRUE, size=7));
df;
#>   name   int        num      fold        lfc       pvalue hit
#> 1    A     4  0.7349388 -9.106049 -3.1868252 2.780730e-02   0
#> 2    B    16  5.3131471 -4.218488 -2.0767258 6.122279e-01   1
#> 3    C    64  9.5543380 -1.395160 -0.4804306 3.146665e-03  -1
#> 4    D   256 15.2240818  1.016424  0.0235028 1.079898e-06  -1
#> 5    E  1024 18.3598138  2.060645  1.0430959 5.027544e-05   0
#> 6    F  4096 22.4007715  5.945047  2.5716883 3.366732e-07   0
#> 7    G 16384 26.1106827  8.898972  3.1536387 8.554139e-09   1
# write to tempfile for examples
if (check_pkg_installed("openxlsx")) {
   out_xlsx <- tempfile(pattern="writeOpenxlsx_", fileext=".xlsx")
   writeOpenxlsx(x=df,
      file=out_xlsx,
      sheetName="jamba_test",
      append=FALSE);
   # now read it back
   df_list <- readOpenxlsx(xlsx=out_xlsx);
   df_list[[1]]
}
#>   name   int        num      fold        lfc       pvalue hit
#> 1    A     4  0.7349388 -9.106049 -3.1868252 2.780730e-02   0
#> 2    B    16  5.3131471 -4.218488 -2.0767258 6.122279e-01   1
#> 3    C    64  9.5543380 -1.395160 -0.4804306 3.146665e-03  -1
#> 4    D   256 15.2240818  1.016424  0.0235028 1.079898e-06  -1
#> 5    E  1024 18.3598138  2.060645  1.0430959 5.027544e-05   0
#> 6    F  4096 22.4007715  5.945047  2.5716883 3.366732e-07   0
#> 7    G 16384 26.1106827  8.898972  3.1536387 8.554139e-09   1
```
