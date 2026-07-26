# Paste data.frame rows into an ordered factor

Paste data.frame rows into an ordered factor

## Usage

``` r
pasteByRowOrdered(
  x,
  sep = "_",
  na.rm = TRUE,
  condenseBlanks = TRUE,
  includeNames = FALSE,
  keepOrder = FALSE,
  byCols = seq_len(ncol(x)),
  na.last = TRUE,
  ...
)
```

## Arguments

- x:

  `data.frame`

- sep:

  `character` separator to use between columns

- na.rm:

  `logical` whether to remove NA values, or include them as "NA"

- condenseBlanks:

  `logical` whether to condense blank or empty values without including
  an extra delimiter between columns.

- includeNames:

  `logical` whether to include the colname delimited prior to the value,
  using sepName as the delimiter.

- keepOrder:

  `logical` indicating whether non-factor columns should order factor
  levels based upon the existing order of unique items. This option is
  intended for `data.frame` whose columns are already sorted in proper
  order, but where columns are not `factor` with appropriate factor
  levels. Note that even when `keepOrder=TRUE` all existing `factor`
  columns will honor the order of factor levels already present in those
  columns.

- byCols:

  `integer` or `character` passed to
  [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md).
  This argument defines the order of columns sorted by
  [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md),
  and does not affect the order of columns pasted. Columns are always
  pasted in the same order they appear in `x`. This argument `byCols`
  was previously passed via `...` but is added here to make this
  connection more direct.

- na.last:

  `logical` passed to
  [`base::factor()`](https://rdrr.io/r/base/factor.html) to determine
  whether `NA` values are first or last in factor level order.

- ...:

  additional arguments are passed to
  [`jamba::pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md),
  and to
  [`jamba::mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md).

## Value

`factor` vector whose levels are defined by existing factor levels, then
by sorted values.

## Details

This function is an extension to
[`jamba::pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md)
which pastes rows from a `data.frame` into a character vector. This
function defines factor levels by running
`jamba::mixedSortDF(unique(x))` and calling
[`jamba::pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md)
on the result. Therefore the original order of the input `x` is
maintained while the factor levels are based upon the appropriate
column-based sort.

Note that the `...` additional arguments are passed to
[`jamba::mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
to customize the column-based sort order, used to define factor levels.
A good way to test the order of factors is to run
`jamba::mixedSortDF(unique(x))` with appropriate arguments, and confirm
the rows are ordered as expected.

Note also that
[`jamba::mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
uses
[`jamba::mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
which itself performs alphanumeric sort in order to keep values in
proper numeric order where possible.

## See also

Other jam string functions:
[`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md),
[`breaksByVector()`](https://jmw86069.github.io/jamba/reference/breaksByVector.md),
[`fillBlanks()`](https://jmw86069.github.io/jamba/reference/fillBlanks.md),
[`formatInt()`](https://jmw86069.github.io/jamba/reference/formatInt.md),
[`gsubOrdered()`](https://jmw86069.github.io/jamba/reference/gsubOrdered.md),
[`gsubs()`](https://jmw86069.github.io/jamba/reference/gsubs.md),
[`makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md),
[`nameVector()`](https://jmw86069.github.io/jamba/reference/nameVector.md),
[`nameVectorN()`](https://jmw86069.github.io/jamba/reference/nameVectorN.md),
[`padInteger()`](https://jmw86069.github.io/jamba/reference/padInteger.md),
[`padString()`](https://jmw86069.github.io/jamba/reference/padString.md),
[`pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md),
[`sizeAsNum()`](https://jmw86069.github.io/jamba/reference/sizeAsNum.md),
[`tcount()`](https://jmw86069.github.io/jamba/reference/tcount.md),
[`ucfirst()`](https://jmw86069.github.io/jamba/reference/ucfirst.md)

## Examples

``` r
f <- LETTERS;
df <- data.frame(A=f[rep(1:3, each=2)],
   B=c(NA, f[3]),
   C=c(NA, NA, f[2]))
df
#>   A    B    C
#> 1 A <NA> <NA>
#> 2 A    C <NA>
#> 3 B <NA>    B
#> 4 B    C <NA>
#> 5 C <NA> <NA>
#> 6 C    C    B

# note that output is consistent with mixedSortDF()
jamba::mixedSortDF(df)
#>   A    B    C
#> 2 A    C <NA>
#> 1 A <NA> <NA>
#> 4 B    C <NA>
#> 3 B <NA>    B
#> 6 C    C    B
#> 5 C <NA> <NA>
jamba::pasteByRowOrdered(df)
#>     1     2     3     4     5     6 
#>     A   A_C   B_B   B_C     C C_C_B 
#> Levels: A_C A B_C B_B C_C_B C

jamba::mixedSortDF(df, na.last=FALSE)
#>   A    B    C
#> 1 A <NA> <NA>
#> 2 A    C <NA>
#> 3 B <NA>    B
#> 4 B    C <NA>
#> 5 C <NA> <NA>
#> 6 C    C    B
jamba::pasteByRowOrdered(df, na.last=FALSE)
#>     1     2     3     4     5     6 
#>     A   A_C   B_B   B_C     C C_C_B 
#> Levels: A A_C B_B B_C C C_C_B

jamba::mixedSortDF(df, byCols=c(3, 2, 1))
#>   A    B    C
#> 6 C    C    B
#> 3 B <NA>    B
#> 2 A    C <NA>
#> 4 B    C <NA>
#> 1 A <NA> <NA>
#> 5 C <NA> <NA>
jamba::pasteByRowOrdered(df, byCols=c(3, 2, 1))
#>     1     2     3     4     5     6 
#>     A   A_C   B_B   B_C     C C_C_B 
#> Levels: C_C_B B_B A_C B_C A C

df1 <- data.frame(group=rep(c("Control", "ABC1"), each=6),
   time=rep(c("Hour2", "Hour10"), each=3),
   rep=paste0("Rep", 1:3))
# default will sort each column alphanumerically
pasteByRowOrdered(df1)
#>                   1                   2                   3                   4 
#>  Control_Hour2_Rep1  Control_Hour2_Rep2  Control_Hour2_Rep3 Control_Hour10_Rep1 
#>                   5                   6                   7                   8 
#> Control_Hour10_Rep2 Control_Hour10_Rep3     ABC1_Hour2_Rep1     ABC1_Hour2_Rep2 
#>                   9                  10                  11                  12 
#>     ABC1_Hour2_Rep3    ABC1_Hour10_Rep1    ABC1_Hour10_Rep2    ABC1_Hour10_Rep3 
#> 12 Levels: ABC1_Hour2_Rep1 ABC1_Hour2_Rep2 ABC1_Hour2_Rep3 ... Control_Hour10_Rep3

# keepOrder=TRUE will honor existing order of character columns
pasteByRowOrdered(df1, keepOrder=TRUE)
#>                   1                   2                   3                   4 
#>  Control_Hour2_Rep1  Control_Hour2_Rep2  Control_Hour2_Rep3 Control_Hour10_Rep1 
#>                   5                   6                   7                   8 
#> Control_Hour10_Rep2 Control_Hour10_Rep3     ABC1_Hour2_Rep1     ABC1_Hour2_Rep2 
#>                   9                  10                  11                  12 
#>     ABC1_Hour2_Rep3    ABC1_Hour10_Rep1    ABC1_Hour10_Rep2    ABC1_Hour10_Rep3 
#> 12 Levels: Control_Hour2_Rep1 Control_Hour2_Rep2 ... ABC1_Hour10_Rep3
```
