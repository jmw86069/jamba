# case-insensitive grep, returning values

case-insensitive grep, returning values

## Usage

``` r
vigrep(..., value = TRUE, ignore.case = TRUE)
```

## Arguments

- ..., value, ignore.case:

  parameters sent to [`base::grep()`](https://rdrr.io/r/base/grep.html)

## Value

vector of matching values

## Details

This function is a simple wrapper around
[`base::grep()`](https://rdrr.io/r/base/grep.html) which runs in
case-insensitive mode, and returns matching values. It is particularly
helpful when grabbing values from a vector.

## See also

Other jam grep functions:
[`grepls()`](https://jmw86069.github.io/jamba/reference/grepls.md),
[`igrep()`](https://jmw86069.github.io/jamba/reference/igrep.md),
[`igrepHas()`](https://jmw86069.github.io/jamba/reference/igrepHas.md),
[`igrepl()`](https://jmw86069.github.io/jamba/reference/igrepl.md),
[`provigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md),
[`unigrep()`](https://jmw86069.github.io/jamba/reference/unigrep.md),
[`unvigrep()`](https://jmw86069.github.io/jamba/reference/unvigrep.md),
[`vgrep()`](https://jmw86069.github.io/jamba/reference/vgrep.md)

## Examples

``` r
V <- paste0(LETTERS[1:5], LETTERS[4:8]);
vigrep("d", V);
#> [1] "AD" "DG"
```
