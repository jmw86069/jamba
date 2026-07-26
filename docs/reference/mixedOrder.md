# order alphanumeric values keeping numeric values in proper order

order alphanumeric values keeping numeric values in proper order

## Usage

``` r
mixedOrder(
  x,
  ...,
  blanksFirst = TRUE,
  na.last = NAlast,
  keepNegative = FALSE,
  keepInfinite = FALSE,
  keepDecimal = FALSE,
  ignore.case = TRUE,
  useCaseTiebreak = TRUE,
  honorFactor = FALSE,
  returnDebug = FALSE,
  returnType = c("order", "rank"),
  NAlast = TRUE,
  verbose = FALSE,
  debug = FALSE
)
```

## Arguments

- x:

  input vector

- ...:

  additional parameters are sent to `mixedOrder()`.

- blanksFirst:

  `logical` whether to order blank entries before entries containing a
  value.

- na.last:

  `logical` whether to move NA entries to the end of the sort. When
  `na.last=TRUE` then `NA` values will always be last, even following
  blanks and infinite values. When `na.last=FALSE` then `NA` values will
  always be first, even before blanks and negative infinite values.

- keepNegative:

  `logical` whether to keep '-' associated with adjacent numeric values,
  in order to sort them as negative values. Note that
  `keepNegative=TRUE` also forces `keepDecimal=TRUE`, and enables
  matching of scientific notation such as `-1.23e-10` as a numeric
  value. When `keepNegative=FALSE` the dash `"-"` is treated as a common
  delimiter.

- keepInfinite:

  `logical` whether to allow "Inf" in the input `x` to be considered a
  numeric infinite value. Note that `"-Inf"` is only treated as a
  negative infinite value when `keepNegative=TRUE`. Also note that
  `"Inf"` is only recognized as infinite when it appears between
  non-character delimiters, and not part of a larger character string
  like `"Information"`. Be careful with `keepInfinite=TRUE` when sorting
  gene symbols, there are gene symbols like `"Inf3"` which should not be
  sorted as infinite. Lastly, infinite values are sorted at the end,
  notably after all character values which differs from some mixed
  sorting algorithms.

- keepDecimal:

  `logical` whether to keep the decimal in numbers, sorting as a true
  number and not as a version number. By default
  ``` keepDecimal=FALSE``, which means "v1.200" will be ordered after "v1.30", since it considers  ```"1.200"`effectively as`1`and`200`, and `"1.30"`effectively as`1`and`30`. When `keepDecimal=TRUE`, the numeric sort orders `"v1.200"`before`"v1.30"\`.

- ignore.case:

  `logical` whether to ignore uppercase and lowercase characters when
  defining the sort order.

- useCaseTiebreak:

  `logical` indicating whether to break ties when `ignore.case=TRUE`,
  using mixed case as a tiebreaker.

- honorFactor:

  `logical` indicating whether to honor the order of `levels` if the
  input `x` is a `factor`. The default `honorFactor=FALSE` is to
  maintain consistent legacy behavior. The purpose of this function is
  to enable alphanumeric sorting, which is not the purpose of sorting by
  factor levels.

- returnDebug:

  `logical` indicating whether to include additional debug info as
  attributes.

- returnType:

  `character` string to define the return type:

  - "order": returns `integer` order, equivalent to
    [`order()`](https://rdrr.io/r/base/order.html)

  - "rank": returns `integer` rank, equivalent to
    [`rank()`](https://rdrr.io/r/base/rank.html)

- NAlast:

  `logical` DEPRECATED in favor of `na.last` for consistency with other
  base R functions.

- verbose:

  `logical` whether to print verbose output.

- debug:

  `logical` indicating whether to return intermediate data useful only
  for debugging purposes.

## Value

`integer` vector of orders derived from x, or when `returnType="rank"`
an integer vector of ranks allowing ties. The rank is therefore valid
for use in chains, such as multiple columns of a `data.frame`.

## Details

This function is a refactor of `gtools` mixedorder() which was the
source of inspiration for this function, thanks to Gregory R. Warnes!
This function was designed to improve the efficiency for large vectors,
and to handle special cases slightly differently. It was driven by some
need to sort gene symbols, and miRNA symbols in numeric order, for
example:

- test set::

  miR-12,miR-1,miR-122,miR-1b,miR-1a,miR-2

- `sort`::

  miR-1,miR-12,miR-122,miR-1a,miR-1b,miR-2

- [`gtools::mixedsort`](https://rdrr.io/pkg/gtools/man/mixedsort.html)::

  miR-122,miR-12,miR-2,miR-1,miR-1a,miR-1b

- `mixedSort`::

  miR-1,miR-1a,miR-1b,miR-2,miR-12,miR-122

This function does not by default consider negative numbers as negative,
instead it treats '-' as a delimiter, unless keepNegative=TRUE.

When `keepNegative=TRUE` this function also recognizes scientific
notation, for example `"1.23e-2"` will be treated as numeric `0.0123`.
Note that `keepNegative=TRUE` also forces `keepDecimal=TRUE`.

When `keepDecimal=TRUE` this function maintains numeric values that
include one `"."`.

This function is the core of a family of mixedSort functions:

- [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md):

  Applies `mixedOrder()` to an input vector.

- [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md):

  Applies `mixedOrder()` to a list of vectors, returning the list where
  each vector is independently sorted.

- [`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md):

  Applies `mixedOrder()` to each column of a `data.frame` or comparable
  object, optionally specifying the order of columns used during the
  sort.

Extra thanks to Gregory R. Warnes for the `gtools` mixedorder() that
proved to be so useful it ultimately inspired this function.

## See also

[`gtools::mixedorder()`](https://rdrr.io/pkg/gtools/man/mixedsort.html),
[`gtools::mixedsort()`](https://rdrr.io/pkg/gtools/man/mixedsort.html)

Other jam sort functions:
[`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md),
[`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md),
[`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md),
[`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md)

## Examples

``` r
x <- c("miR-12","miR-1","miR-122","miR-1b", "miR-1a","miR-2");
mixedOrder(x);
#> [1] 2 5 4 6 1 3
x[mixedOrder(x)];
#> [1] "miR-1"   "miR-1a"  "miR-1b"  "miR-2"   "miR-12"  "miR-122"
mixedSort(x);
#> [1] "miR-1"   "miR-1a"  "miR-1b"  "miR-2"   "miR-12"  "miR-122"
order(x);
#> [1] 2 1 3 5 4 6
x[order(x)];
#> [1] "miR-1"   "miR-12"  "miR-122" "miR-1a"  "miR-1b"  "miR-2"  
sort(x);
#> [1] "miR-1"   "miR-12"  "miR-122" "miR-1a"  "miR-1b"  "miR-2"  

## Complex example including NA, blanks, and infinite "Inf"
x <- c("Inf",
   "+Inf12",
   NA,
   "-Inf14",
   "-",
   "---",
   "Jnf12",
   "Hnf12",
   "--",
   "Information");
## By default, strings are sorted as-is, "Hnf" before "Inf" before "Jnf"
## blanks are first, NA values are last
x[mixedOrder(x)];
#>  [1] "-"           "--"          "---"         "Hnf12"       "+Inf12"     
#>  [6] "-Inf14"      "Inf"         "Information" "Jnf12"       NA           

## blanks are last, but before NA values which are also last
x[mixedOrder(x, blanksFirst=FALSE)];
#>  [1] "Hnf12"       "+Inf12"      "-Inf14"      "Inf"         "Information"
#>  [6] "Jnf12"       "-"           "--"          "---"         NA           

## Recognize infinite, but not the negative sign
## Now infinite values are at the end, ordered by the number that follows.
x[mixedOrder(x, blanksFirst=FALSE, keepInfinite=TRUE)]
#>  [1] "Hnf12"       "Information" "Jnf12"       "+Inf12"      "-Inf14"     
#>  [6] "Inf"         "-"           "--"          "---"         NA           

## Now also recognize negative infinite values,
## which puts "-Inf14" at the very beginning.
x[mixedOrder(x, blanksFirst=FALSE, keepInfinite=TRUE, keepNegative=TRUE)]
#>  [1] "-Inf14"      "Hnf12"       "Information" "Jnf12"       "+Inf12"     
#>  [6] "Inf"         "-"           "--"          "---"         NA           

# test factor level order
factor1 <- factor(c("Cnot9", "Cnot8", "Cnot10"))
sort(factor1)
#> [1] Cnot10 Cnot8  Cnot9 
#> Levels: Cnot10 Cnot8 Cnot9
mixedSort(factor1)
#> [1] Cnot8  Cnot9  Cnot10
#> Levels: Cnot10 Cnot8 Cnot9
factor1[mixedOrder(factor1)]
#> [1] Cnot8  Cnot9  Cnot10
#> Levels: Cnot10 Cnot8 Cnot9
factor1[mixedOrder(factor1, honorFactor=TRUE)]
#> [1] Cnot10 Cnot8  Cnot9 
#> Levels: Cnot10 Cnot8 Cnot9
```
