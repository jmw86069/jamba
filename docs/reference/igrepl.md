# case-insensitive logical grepl

case-insensitive logical grepl

## Usage

``` r
igrepl(..., ignore.case = TRUE)
```

## Arguments

- ..., ignore.case:

  parameters sent to [`base::grep()`](https://rdrr.io/r/base/grep.html)

## Value

`logical` vector indicating pattern match

## Details

This function is a simple wrapper around
[`base::grepl()`](https://rdrr.io/r/base/grep.html) which runs in
case-insensitive mode simply by adding default `ignore.case=TRUE`. It is
mainly used for convenience.

## See also

Other jam grep functions:
[`grepls()`](https://jmw86069.github.io/jamba/reference/grepls.md),
[`igrep()`](https://jmw86069.github.io/jamba/reference/igrep.md),
[`igrepHas()`](https://jmw86069.github.io/jamba/reference/igrepHas.md),
[`provigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md),
[`unigrep()`](https://jmw86069.github.io/jamba/reference/unigrep.md),
[`unvigrep()`](https://jmw86069.github.io/jamba/reference/unvigrep.md),
[`vgrep()`](https://jmw86069.github.io/jamba/reference/vgrep.md),
[`vigrep()`](https://jmw86069.github.io/jamba/reference/vigrep.md)

## Examples

``` r
V <- paste0(LETTERS[1:5], LETTERS[4:8]);
ig1 <- grepl("D", V);
ig2 <- igrepl("D", V);
ig3 <- grepl("d", V);
ig4 <- igrepl("d", V);
data.frame(V,
   grepl_D=ig1,
   grepl_d=ig3,
   igrepl_D=ig2,
   igrepl_d=ig4);
#>    V grepl_D grepl_d igrepl_D igrepl_d
#> 1 AD    TRUE   FALSE     TRUE     TRUE
#> 2 BE   FALSE   FALSE    FALSE    FALSE
#> 3 CF   FALSE   FALSE    FALSE    FALSE
#> 4 DG    TRUE   FALSE     TRUE     TRUE
#> 5 EH   FALSE   FALSE    FALSE    FALSE
```
