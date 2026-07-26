# convert date to age in days

convert date to age in days

## Usage

``` r
dateToDaysOld(testDate, nowDate = Sys.Date(), units = "days", ...)
```

## Arguments

- testDate:

  `character` date recognized by
  [`asDate()`](https://jmw86069.github.io/jamba/reference/asDate.md),
  representing the test date.

- nowDate:

  `character` date recognized by
  [`asDate()`](https://jmw86069.github.io/jamba/reference/asDate.md),
  representing the reference date, by default the current day.

- units:

  `character` indicating the units, as used by
  [`difftime()`](https://rdrr.io/r/base/difftime.html).

- ...:

  additional parameters are ignored.

## Value

integer value with the number of calendar days before the current date,
or the `nowDate` if supplied.

## See also

Other jam date functions:
[`asDate()`](https://jmw86069.github.io/jamba/reference/asDate.md),
[`getDate()`](https://jmw86069.github.io/jamba/reference/getDate.md)

## Examples

``` r
dateToDaysOld("23aug2007")
#> [1] 6911
```
