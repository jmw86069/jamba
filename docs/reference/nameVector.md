# assign unique names for a vector

assign unique names for a vector

## Usage

``` r
nameVector(x, y = NULL, makeNamesFunc = makeNames, ...)
```

## Arguments

- x:

  `character` vector, or `data.frame` or equivalent (matrix, or tibble)
  with two columns, the second column is used to name values in the
  first column.

- y:

  `character` or NULL, with names. If NULL then x is used. Note that y
  is recycled to the length of x, prior to being sent to the
  makeNamesFunc. In fringe cases, y can be a matrix, data.frame, or
  tibble, in which case
  [`pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md)
  will be used to create a character string to be used for vector names.
  Note this case is activated only when x is not a two column matrix,
  data.frame, or tibble.

- makeNamesFunc:

  `function` to make names unique, by default
  [`makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md)
  which ensures names are unique.

- ...:

  passed to `makeNamesFunc`, or to
  [`pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md)
  if y is a two column data.frame, matrix, or tibble. Thus, `sep` can be
  defined here as a delimiter between column values.

## Value

vector with names defined

## Details

This function assigns unique names to a vector, if necessary it runs
[`makeNames`](https://jmw86069.github.io/jamba/reference/makeNames.md)
to create unique names. It differs from
[`setNames`](https://rdrr.io/r/stats/setNames.html) in that it ensures
names are unique, and when no names are supplied, it uses the vector
itself to define names. It is helpful to run this function inside an
[`lapply`](https://rdrr.io/r/base/lapply.html) function call, which by
default maintains names, but does not assign names if the input data did
not already have them.

When used with a data.frame, it is particularly convenient to pull out a
named vector of values. For example, log2 fold changes by gene, where
the gene symbols are the name of the vector.

`nameVector(genedata[,c("Gene","log2FC")])`

## See also

Other jam string functions:
[`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md),
[`breaksByVector()`](https://jmw86069.github.io/jamba/reference/breaksByVector.md),
[`fillBlanks()`](https://jmw86069.github.io/jamba/reference/fillBlanks.md),
[`formatInt()`](https://jmw86069.github.io/jamba/reference/formatInt.md),
[`gsubOrdered()`](https://jmw86069.github.io/jamba/reference/gsubOrdered.md),
[`gsubs()`](https://jmw86069.github.io/jamba/reference/gsubs.md),
[`makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md),
[`nameVectorN()`](https://jmw86069.github.io/jamba/reference/nameVectorN.md),
[`padInteger()`](https://jmw86069.github.io/jamba/reference/padInteger.md),
[`padString()`](https://jmw86069.github.io/jamba/reference/padString.md),
[`pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md),
[`pasteByRowOrdered()`](https://jmw86069.github.io/jamba/reference/pasteByRowOrdered.md),
[`sizeAsNum()`](https://jmw86069.github.io/jamba/reference/sizeAsNum.md),
[`tcount()`](https://jmw86069.github.io/jamba/reference/tcount.md),
[`ucfirst()`](https://jmw86069.github.io/jamba/reference/ucfirst.md)

## Examples

``` r
# it generally just creates names from the vector values
nameVector(LETTERS[1:5]);
#>   A   B   C   D   E 
#> "A" "B" "C" "D" "E" 

# if values are replicated, the makeNames() function makes them unique
V <- rep(LETTERS[1:5], each=3);
nameVector(V);
#> A_v1 A_v2 A_v3 B_v1 B_v2 B_v3 C_v1 C_v2 C_v3 D_v1 D_v2 D_v3 E_v1 E_v2 E_v3 
#>  "A"  "A"  "A"  "B"  "B"  "B"  "C"  "C"  "C"  "D"  "D"  "D"  "E"  "E"  "E" 

# for a two-column data.frame, it creates a named vector using
# the values in the first column, and names in the second column.
df <- data.frame(seq_along(V), V);
df;
#>    seq_along.V. V
#> 1             1 A
#> 2             2 A
#> 3             3 A
#> 4             4 B
#> 5             5 B
#> 6             6 B
#> 7             7 C
#> 8             8 C
#> 9             9 C
#> 10           10 D
#> 11           11 D
#> 12           12 D
#> 13           13 E
#> 14           14 E
#> 15           15 E
nameVector(df);
#> A_v1 A_v2 A_v3 B_v1 B_v2 B_v3 C_v1 C_v2 C_v3 D_v1 D_v2 D_v3 E_v1 E_v2 E_v3 
#>    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 

# Lastly, admittedly a fringe case, it can take a multi-column data.frame
# to generate labels:
nameVector(V, df);
#>  1_A  2_A  3_A  4_B  5_B  6_B  7_C  8_C  9_C 10_D 11_D 12_D 13_E 14_E 15_E 
#>  "A"  "A"  "A"  "B"  "B"  "B"  "C"  "C"  "C"  "D"  "D"  "D"  "E"  "E"  "E" 
```
