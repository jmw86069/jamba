# case-insensitive grep

case-insensitive grep

## Usage

``` r
igrep(..., ignore.case = TRUE)
```

## Arguments

- ..., ignore.case:

  parameters sent to [`base::grep()`](https://rdrr.io/r/base/grep.html)

## Value

vector of matching indices

## Details

This function is a simple wrapper around
[`base::grep()`](https://rdrr.io/r/base/grep.html) which runs in
case-insensitive mode. It is mainly used to save keystrokes, but is
consistently named alongside
[`vgrep`](https://jmw86069.github.io/jamba/reference/vgrep.md) and
[`vigrep`](https://jmw86069.github.io/jamba/reference/vigrep.md).

## See also

Other jam grep functions:
[`grepls()`](https://jmw86069.github.io/jamba/reference/grepls.md),
[`igrepHas()`](https://jmw86069.github.io/jamba/reference/igrepHas.md),
[`igrepl()`](https://jmw86069.github.io/jamba/reference/igrepl.md),
[`provigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md),
[`unigrep()`](https://jmw86069.github.io/jamba/reference/unigrep.md),
[`unvigrep()`](https://jmw86069.github.io/jamba/reference/unvigrep.md),
[`vgrep()`](https://jmw86069.github.io/jamba/reference/vgrep.md),
[`vigrep()`](https://jmw86069.github.io/jamba/reference/vigrep.md)

## Examples

``` r
V <- paste0(LETTERS[1:5], LETTERS[4:8]);
igrep("D", V);
#> [1] 1 4
igrep("d", V);
#> [1] 1 4
vigrep("d", V);
#> [1] "AD" "DG"
```
