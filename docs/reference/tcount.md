# frequency of entries, ordered by frequency

frequency of entries, ordered by frequency

## Usage

``` r
tcount(
  x,
  minCount = NULL,
  doSort = TRUE,
  maxCount = NULL,
  nameSortFunc = sort,
  ...
)
```

## Arguments

- x:

  `character`, `numeric`, `factor` vector input to use when calculating
  frequencies.

- minCount:

  optional `integer` minimum frequency, any results with fewer counts
  observed will be omitted from results.

- doSort:

  `logical` whether to sort results decreasing by frequency.

- maxCount:

  optional `integer` maximum frequency for returned results.

- nameSortFunc:

  `function` used to sort results after sorting by frequency. For
  example, one might use
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md).
  If `nameSortFunc=NULL` then no name sort will be applied.

- ...:

  additional parameters are ignored.

## Value

`integer` vector of counts, named by the unique input values in `x`.

## Details

This function mimics output from
[`table()`](https://rdrr.io/r/base/table.html) with two key differences.
It sorts the results by decreasing frequency, and optionally filters
results for a minimum frequency. It is effective when checking for
duplicate values, and ordering them by the number of occurrences.

This function is useful when working with large vectors of gene
identifiers, where it is not always obvious whether genes are replicated
in a particular technological assay. Transcript microarrays for example,
can contain many replicated genes, but often only a handful of genes are
highly replicated, while the rest are present only once or twice on the
array.

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
[`pasteByRowOrdered()`](https://jmw86069.github.io/jamba/reference/pasteByRowOrdered.md),
[`sizeAsNum()`](https://jmw86069.github.io/jamba/reference/sizeAsNum.md),
[`ucfirst()`](https://jmw86069.github.io/jamba/reference/ucfirst.md)

## Examples

``` r
testVector <- rep(c("one", "two", "three", "four"), c(1:4));
tcount(testVector);
#>  four three   two   one 
#>     4     3     2     1 
tcount(testVector, minCount=2);
#>  four three   two 
#>     4     3     2 
```
