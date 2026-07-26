# sort alphanumeric values keeping numeric values in proper order

sort alphanumeric values keeping numeric values in proper order

## Usage

``` r
mixedSort(
  x,
  blanksFirst = TRUE,
  na.last = NAlast,
  keepNegative = FALSE,
  keepInfinite = FALSE,
  keepDecimal = FALSE,
  ignore.case = TRUE,
  useCaseTiebreak = TRUE,
  honorFactor = FALSE,
  sortByName = FALSE,
  verbose = FALSE,
  NAlast = TRUE,
  ...
)
```

## Arguments

- x:

  `vector`

- blanksFirst:

  `logical` whether to order blank entries before entries containing a
  value.

- na.last:

  `logical` indicating whether to move NA entries at the end of the
  sort.

- keepNegative:

  `logical` whether to keep '-' associated with adjacent numeric values,
  in order to sort them as negative values.

- keepInfinite:

  `logical` whether to allow "Inf" to be considered a numeric infinite
  value.

- keepDecimal:

  `logical` whether to keep the decimal in numbers, sorting as a true
  number and not as a version number. By default keepDecimal=FALSE,
  which means "v1.200" should be ordered before "v1.30". When
  keepDecimal=TRUE, the numeric sort considers only "1.2" and "1.3" and
  sorts in that order.

- ignore.case:

  `logical` whether to ignore uppercase and lowercase characters when
  defining the sort order. Note that when `x` is `factor` the factor
  levels are converted using `unique(toupper(levels(x)))`, therefore the
  values in `x` will be sorted by factor level.

- useCaseTiebreak:

  `logical` indicating whether to break ties when `ignore.case=TRUE`,
  using mixed case as a tiebreaker.

- honorFactor:

  `logical`, default TRUE, indicating whether to honor factor level
  order in the output, otherwise when FALSE it sorts as `character`.

- sortByName:

  `logical` whether to sort the vector x by names(x) instead of sorting
  by x itself.

- verbose:

  `logical` whether to print verbose output.

- NAlast:

  `logical` deprecated in favor of argument `na.last` for consistency
  with [`base::sort()`](https://rdrr.io/r/base/sort.html).

- ...:

  additional parameters are sent to
  [`mixedOrder`](https://jmw86069.github.io/jamba/reference/mixedOrder.md).

## Value

`vector` of values from argument `x`, ordered by
[`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md).
The output class should match `class(x)`.

## Details

This function is a refactor of `gtools` mixedsort(), a clever bit of R
coding from the `gtools` package. It was extended to make it slightly
faster, and to handle special cases slightly differently. It was driven
by the need to sort gene symbols, miRNA symbols, chromosome names, all
with proper numeric order, for example:

- test set::

  miR-12,miR-1,miR-122,miR-1b,mir-1a

- gtools::mixedsort::

  miR-122,miR-12,miR-1,miR-1a,mir-1b

- mixedSort::

  miR-1,miR-1a,miR-1b,miR-12,miR-122

The function does not by default recognize negative numbers as negative,
instead it treats '-' as a delimiter, unless `keepNegative=TRUE`.

This function also attempts to maintain '.' as part of a decimal number,
which can be problematic when sorting IP addresses, for example.

This function is really just a wrapper function for
[`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md),
which does the work of defining the appropriate order.

The sort logic is roughly as follows:

- Split each term into alternating chunks containing `character` or
  `numeric` substrings, split across columns in a matrix.

- Apply appropriate `ignore.case` logic to the character substrings,
  effectively applying [`toupper()`](https://rdrr.io/r/base/chartr.html)
  on substrings

- Define rank order of character substrings in each matrix column,
  maintaining ties to be resolved in subsequent columns.

- Convert `character` to `numeric` ranks via `factor` intermediate,
  defined higher than the highest `numeric` substring value.

- When `ignore.case=TRUE` and `useCaseTiebreak=TRUE`, an additional
  tiebreaker column is defined using the `character` substring values
  without applying [`toupper()`](https://rdrr.io/r/base/chartr.html).

- A final tiebreaker column is the input string itself, with
  [`toupper()`](https://rdrr.io/r/base/chartr.html) applied when
  `ignore.case=TRUE`.

- Apply order across all substring columns.

Therefore, some expected behaviors:

- When `ignore.case=TRUE` and `useCaseTiebreak=TRUE` (default for both)
  the input data is ordered without regard to case, then the tiebreaker
  applies case-specific sort criteria to the final product. This logic
  is very close to default [`sort()`](https://rdrr.io/r/base/sort.html)
  except for the handling of internal `numeric` values inside each
  string.

## See also

Other jam sort functions:
[`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md),
[`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md),
[`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md),
[`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md)

## Examples

``` r
x <- c("miR-12","miR-1","miR-122","miR-1b", "miR-1a", "miR-2");
sort(x);
#> [1] "miR-1"   "miR-12"  "miR-122" "miR-1a"  "miR-1b"  "miR-2"  
mixedSort(x);
#> [1] "miR-1"   "miR-1a"  "miR-1b"  "miR-2"   "miR-12"  "miR-122"

# test honorFactor
mixedSort(factor(c("Cnot9", "Cnot8", "Cnot10")))
#> [1] Cnot8  Cnot9  Cnot10
#> Levels: Cnot10 Cnot8 Cnot9
mixedSort(factor(c("Cnot9", "Cnot8", "Cnot10")), honorFactor=TRUE)
#> [1] Cnot10 Cnot8  Cnot9 
#> Levels: Cnot10 Cnot8 Cnot9

# test ignore.case
mixedSort(factor(c("Cnot9", "Cnot8", "CNOT9", "Cnot10")))
#> [1] Cnot8  CNOT9  Cnot9  Cnot10
#> Levels: CNOT9 Cnot10 Cnot8 Cnot9
mixedSort(factor(c("CNOT9", "Cnot8", "Cnot9", "Cnot10")))
#> [1] Cnot8  CNOT9  Cnot9  Cnot10
#> Levels: CNOT9 Cnot10 Cnot8 Cnot9
mixedSort(factor(c("Cnot9", "Cnot8", "CNOT9", "Cnot10")), ignore.case=FALSE)
#> [1] CNOT9  Cnot8  Cnot9  Cnot10
#> Levels: CNOT9 Cnot10 Cnot8 Cnot9
mixedSort(factor(c("Cnot9", "Cnot8", "CNOT9", "Cnot10")), ignore.case=TRUE)
#> [1] Cnot8  CNOT9  Cnot9  Cnot10
#> Levels: CNOT9 Cnot10 Cnot8 Cnot9

mixedSort(factor(c("Cnot9", "Cnot8", "CNOT9", "Cnot10")), useCaseTiebreak=TRUE)
#> [1] Cnot8  CNOT9  Cnot9  Cnot10
#> Levels: CNOT9 Cnot10 Cnot8 Cnot9
mixedSort(factor(c("CNOT9", "Cnot8", "Cnot9", "Cnot10")), useCaseTiebreak=FALSE)
#> [1] Cnot8  CNOT9  Cnot9  Cnot10
#> Levels: CNOT9 Cnot10 Cnot8 Cnot9
```
