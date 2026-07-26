# case-insensitive grep, returning unmatched indices

case-insensitive grep, returning unmatched indices

## Usage

``` r
unigrep(..., ignore.case = TRUE, invert = TRUE)
```

## Arguments

- ..., ignore.case, invert:

  parameters sent to [`base::grep()`](https://rdrr.io/r/base/grep.html)

## Value

vector of non-matching indices

## Details

This function is a simple wrapper around
[`base::grep()`](https://rdrr.io/r/base/grep.html) which runs in
case-insensitive mode, and returns unmatched entries. It is mainly used
to save keystrokes, but is consistently named alongside
[`vgrep`](https://jmw86069.github.io/jamba/reference/vgrep.md) and
[`vigrep`](https://jmw86069.github.io/jamba/reference/vigrep.md), and
quite helpful for writing concise code.

## See also

Other jam grep functions:
[`grepls()`](https://jmw86069.github.io/jamba/reference/grepls.md),
[`igrep()`](https://jmw86069.github.io/jamba/reference/igrep.md),
[`igrepHas()`](https://jmw86069.github.io/jamba/reference/igrepHas.md),
[`igrepl()`](https://jmw86069.github.io/jamba/reference/igrepl.md),
[`provigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md),
[`unvigrep()`](https://jmw86069.github.io/jamba/reference/unvigrep.md),
[`vgrep()`](https://jmw86069.github.io/jamba/reference/vgrep.md),
[`vigrep()`](https://jmw86069.github.io/jamba/reference/vigrep.md)

## Examples

``` r
V <- paste0(LETTERS[1:5], LETTERS[4:8]);
unigrep("D", V);
#> [1] 2 3 5
igrep("D", V);
#> [1] 1 4
```
