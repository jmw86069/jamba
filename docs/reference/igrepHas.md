# vector contains any case-insensitive grep match

vector contains any case-insensitive grep match

## Usage

``` r
igrepHas(
  pattern,
  x = NULL,
  ignore.case = TRUE,
  minCount = 1,
  naToBlank = FALSE,
  ...
)
```

## Arguments

- pattern:

  the grep pattern to use with
  [`base::grep()`](https://rdrr.io/r/base/grep.html)

- x:

  vector to use in the grep

- ignore.case:

  logical default TRUE, meaning the grep will be performed in
  case-insensitive mode.

- minCount:

  integer minimum number of matches required to return TRUE.

- naToBlank:

  logical whether to convert NA to blank, instead of allowing grep to
  handle NA values as-is.

- ...:

  additional arguments are ignored.

## Value

logical indicating whether the grep match criteria were met, TRUE
indicates the grep pattern was present in minCount or more number of
entries.

## Details

This function checks the input vector for any elements matching the grep
pattern. The grep is performed case-insensitive (igrep). This function
is particularly useful when checking function arguments or object class,
where the class(a) might return multiple values, or where the name of
the class might be slightly different than expected, e.g. data.frame,
data_frame, DataFrame.

## See also

[`base::grep()`](https://rdrr.io/r/base/grep.html)

Other jam grep functions:
[`grepls()`](https://jmw86069.github.io/jamba/reference/grepls.md),
[`igrep()`](https://jmw86069.github.io/jamba/reference/igrep.md),
[`igrepl()`](https://jmw86069.github.io/jamba/reference/igrepl.md),
[`provigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md),
[`unigrep()`](https://jmw86069.github.io/jamba/reference/unigrep.md),
[`unvigrep()`](https://jmw86069.github.io/jamba/reference/unvigrep.md),
[`vgrep()`](https://jmw86069.github.io/jamba/reference/vgrep.md),
[`vigrep()`](https://jmw86069.github.io/jamba/reference/vigrep.md)

## Examples

``` r
a <- c("data.frame","data_frame","tibble","tbl");
igrepHas("Data.*Frame", a);
#> [1] TRUE
igrepHas("matrix", a);
#> [1] FALSE
```
