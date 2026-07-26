# convert date DDmmmYYYY to Date

convert date DDmmmYYYY to Date

## Usage

``` r
asDate(getDateValues, dateFormat = "%d%b%Y", ...)
```

## Arguments

- getDateValues:

  `character` date, in format recognized by dateFormat

- dateFormat:

  `character` string representing the recognized date format, by default
  `"DDmmmYYYY"`, which recognizes `"23aug2007"`.

- ...:

  additional parameters are ignored.

## Value

Date object

## Details

This function converts a text date string to Date object, mainly to
allow date-related math operations, for example
[`difftime`](https://rdrr.io/r/base/difftime.html).

## See also

Other jam date functions:
[`dateToDaysOld()`](https://jmw86069.github.io/jamba/reference/dateToDaysOld.md),
[`getDate()`](https://jmw86069.github.io/jamba/reference/getDate.md)

## Examples

``` r
asDate(getDate());
#>    25jul2026 
#> "2026-07-25" 
```
