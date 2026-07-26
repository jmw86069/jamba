# get simple date string

get simple date string in the format DDmonYYYY such as 17jul2018.

## Usage

``` r
getDate(t = Sys.time(), trim = TRUE, dateFormat = "%d%b%Y", ...)
```

## Arguments

- t:

  current time in an appropriate class such as `"POSIXct"` or
  `"POSIXt"`. The default is output of
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html).

- trim:

  `logical` whether to trim the output of
  [`format()`](https://rdrr.io/r/base/format.html) in the event that
  multiple values are sent for argument `t`.

- dateFormat:

  `character` string representing the recognized date format, by default
  `"DDmmmYYYY"`, which recognizes `"23aug2007"`.

- ...:

  additional parameters sent to
  [`format()`](https://rdrr.io/r/base/format.html).

## Value

`character` vector with simplified date string

## Details

Gets the current date in a simplified text string. Use
[`asDate()`](https://jmw86069.github.io/jamba/reference/asDate.md) to
convert back to Date object.

## See also

Other jam date functions:
[`asDate()`](https://jmw86069.github.io/jamba/reference/asDate.md),
[`dateToDaysOld()`](https://jmw86069.github.io/jamba/reference/dateToDaysOld.md)

## Examples

``` r
getDate();
#> [1] "25jul2026"
```
