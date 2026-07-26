# Convert list of vectors to data.frame with item, value, name

Convert list of vectors to data.frame with item, value, name

## Usage

``` r
list2df(x, makeUnique = TRUE, useVectorNames = TRUE, ...)
```

## Arguments

- x:

  list of vectors

- makeUnique:

  logical indicating whether the data.frame should contain unique rows.

- useVectorNames:

  logical indicating whether vector names should be included in the
  data.frame, if they exist.

- ...:

  additional arguments are ignored.

## Value

`data.frame` with two columns, or three columns when
`useVectorNames=TRUE` and the input `x` contains names.

## Details

This function converts a list of vectors to a tall data.frame with
colnames `item` to indicate the list name, `value` to indicate the
vector value, and `name` to indicate the vector name if
`useVectorNames=TRUE` and if names exist.

## See also

Other jam list functions:
[`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md),
[`heads()`](https://jmw86069.github.io/jamba/reference/heads.md),
[`jam_rapply()`](https://jmw86069.github.io/jamba/reference/jam_rapply.md),
[`mergeAllXY()`](https://jmw86069.github.io/jamba/reference/mergeAllXY.md),
[`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md),
[`rbindList()`](https://jmw86069.github.io/jamba/reference/rbindList.md),
[`relist_named()`](https://jmw86069.github.io/jamba/reference/relist_named.md),
[`rlengths()`](https://jmw86069.github.io/jamba/reference/rlengths.md),
[`sclass()`](https://jmw86069.github.io/jamba/reference/sclass.md),
[`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md),
[`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md),
[`unnestList()`](https://jmw86069.github.io/jamba/reference/unnestList.md)

## Examples

``` r
list2df(list(lower=head(letters, 5), UPPER=head(LETTERS, 10)))
#>     item value
#> 1  lower     a
#> 2  lower     b
#> 3  lower     c
#> 4  lower     d
#> 5  lower     e
#> 6  UPPER     A
#> 7  UPPER     B
#> 8  UPPER     C
#> 9  UPPER     D
#> 10 UPPER     E
#> 11 UPPER     F
#> 12 UPPER     G
#> 13 UPPER     H
#> 14 UPPER     I
#> 15 UPPER     J

list2df(list(lower=nameVector(head(letters, 5)),
   UPPER=nameVector(head(LETTERS, 10))))
#>     item value name
#> 1  lower     a    a
#> 2  lower     b    b
#> 3  lower     c    c
#> 4  lower     d    d
#> 5  lower     e    e
#> 6  UPPER     A    A
#> 7  UPPER     B    B
#> 8  UPPER     C    C
#> 9  UPPER     D    D
#> 10 UPPER     E    E
#> 11 UPPER     F    F
#> 12 UPPER     G    G
#> 13 UPPER     H    H
#> 14 UPPER     I    I
#> 15 UPPER     J    J

list2df(list(lower=nameVector(head(letters, 5)),
   UPPER=nameVector(head(LETTERS, 10))),
   useVectorNames=FALSE)
#>     item value
#> 1  lower     a
#> 2  lower     b
#> 3  lower     c
#> 4  lower     d
#> 5  lower     e
#> 6  UPPER     A
#> 7  UPPER     B
#> 8  UPPER     C
#> 9  UPPER     D
#> 10 UPPER     E
#> 11 UPPER     F
#> 12 UPPER     G
#> 13 UPPER     H
#> 14 UPPER     I
#> 15 UPPER     J
```
