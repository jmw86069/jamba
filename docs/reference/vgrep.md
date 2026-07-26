# grep, returning values

grep, returning values

## Usage

``` r
vgrep(..., value = TRUE, ignore.case = FALSE)
```

## Arguments

- ..., value, ignore.case:

  parameters sent to [`base::grep()`](https://rdrr.io/r/base/grep.html)

## Value

vector of matching values

## Details

This function is a simple wrapper around
[`base::grep()`](https://rdrr.io/r/base/grep.html) which returns
matching values. It is particularly helpful when grabbing values from a
vector, but where the case (uppercase or lowercase) is known.

## See also

Other jam grep functions:
[`grepls()`](https://jmw86069.github.io/jamba/reference/grepls.md),
[`igrep()`](https://jmw86069.github.io/jamba/reference/igrep.md),
[`igrepHas()`](https://jmw86069.github.io/jamba/reference/igrepHas.md),
[`igrepl()`](https://jmw86069.github.io/jamba/reference/igrepl.md),
[`provigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md),
[`unigrep()`](https://jmw86069.github.io/jamba/reference/unigrep.md),
[`unvigrep()`](https://jmw86069.github.io/jamba/reference/unvigrep.md),
[`vigrep()`](https://jmw86069.github.io/jamba/reference/vigrep.md)

## Examples

``` r
V <- paste0(LETTERS[1:5], LETTERS[4:8]);
vgrep("D", V);
#> [1] "AD" "DG"
vgrep("d", V);
#> character(0)
vigrep("d", V);
#> [1] "AD" "DG"
```
