# order alphanumeric values from a list

order alphanumeric values from a list

## Usage

``` r
mmixedOrder(
  ...,
  decreasing = FALSE,
  blanksFirst = TRUE,
  na.last = NAlast,
  keepNegative = FALSE,
  keepInfinite = FALSE,
  keepDecimal = FALSE,
  ignore.case = TRUE,
  useCaseTiebreak = TRUE,
  sortByName = FALSE,
  NAlast = TRUE,
  honorFactor = TRUE,
  verbose = FALSE,
  matrixAsDF = TRUE
)
```

## Arguments

- ...:

  arguments treated as a `list` of vectors to be ordered in proper
  order, based upon the mechanism by
  [`base::order()`](https://rdrr.io/r/base/order.html), and as such
  `data.frame` is equivalent to a `list`.

- decreasing:

  `logical`, default FALSE, used to reverse the sort order.

- blanksFirst, na.last, keepNegative, keepInfinite, keepDecimal,
  ignore.case, useCaseTiebreak, sortByName:

  arguments passed to
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md),
  except `sortByName` which is not passed along.

- NAlast:

  `logical` deprecated in favor of argument `na.last` for consistency
  with [`base::sort()`](https://rdrr.io/r/base/sort.html).

- honorFactor:

  `logical`, default TRUE, used to enforce factor level sort order, when
  FALSE it sorts as `character`.

- verbose:

  `logical` indicating whether to print verbose output, passed as
  `verbose - 1` to
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md).

- matrixAsDF:

  `logical` if `...` supplies only one matrix object, then
  `matrixAsDF=TRUE` will cause it to be converted to a `data.frame`,
  then coerce to a `list` before processing. By default, in the event
  only one matrix object is supplied, this conversion is performed, in
  order to define a sort order based upon each column in order,
  consistent with behavior of `data.frame` input.

## Value

`integer` vector of row orders

## Details

This function is a minor extension to
[`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md),
"multiple
[`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md)",
which accepts `list` input, similar to how
[`base::order()`](https://rdrr.io/r/base/order.html) operates. This
function is mainly useful when sorting something like a `data.frame`,
where ties in column 1 should be maintained then broken by non-equal
values in column 2, and so on.

This function essentially converts any non-numeric column to a factor,
whose levels are sorted using
[`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md).
That factor is converted to numeric value, multiplied by `-1` when
`decreasing=TRUE`. Finally the list of numeric vectors is passed to
[`base::order()`](https://rdrr.io/r/base/order.html).

In fact,
[`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md)
calls this `mmixedOrder()` function, in order to sort a `data.frame`
properly by column.

See
[`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md)
and
[`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
for a better description of how the sort order logic operates.

## See also

Other jam sort functions:
[`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md),
[`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md),
[`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md),
[`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)

## Examples

``` r
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
factor1[mixedOrder(factor1, honorFactor=FALSE)]
#> [1] Cnot8  Cnot9  Cnot10
#> Levels: Cnot10 Cnot8 Cnot9
factor1[mixedOrder(factor1, honorFactor=TRUE)]
#> [1] Cnot10 Cnot8  Cnot9 
#> Levels: Cnot10 Cnot8 Cnot9

factor1[mmixedOrder(list(factor1))]
#> [1] Cnot10 Cnot8  Cnot9 
#> Levels: Cnot10 Cnot8 Cnot9
factor1[mmixedOrder(list(factor1), honorFactor=FALSE)]
#> [1] Cnot8  Cnot9  Cnot10
#> Levels: Cnot10 Cnot8 Cnot9
factor1[mmixedOrder(list(factor1), honorFactor=TRUE)]
#> [1] Cnot10 Cnot8  Cnot9 
#> Levels: Cnot10 Cnot8 Cnot9
```
