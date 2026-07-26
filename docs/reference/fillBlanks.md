# Fill blank entries in a vector

Fill blank entries in a vector

## Usage

``` r
fillBlanks(x, blankGrep = c("[ \t]*"), first = "", ...)
```

## Arguments

- x:

  character vector

- blankGrep:

  vector of grep patterns, or `NA`, indicating the type of entry to be
  considered blank. Each `blankGrep` pattern is searched using
  [`jamba::proigrep()`](https://jmw86069.github.io/jamba/reference/provigrep.md),
  which by default uses case-insensitive regular expression pattern
  matching.

- first:

  options character string intended when the first entry of `x` is
  blank. By default `""` is used.

- ...:

  additional parameters are ignored.

## Value

`character` vector where blank entries are filled with the most recent
non-blank value.

## Details

This function takes a character vector and fills any blank (missing)
entries with the last non-blank entry in the vector. It is intended for
situations like imported 'Excel' data, where there may be one header
value representing a series of cells.

The method used does not loop through the data, and should scale fairly
well with good efficiency even for extremely large vectors.

## See also

Other jam string functions:
[`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md),
[`breaksByVector()`](https://jmw86069.github.io/jamba/reference/breaksByVector.md),
[`formatInt()`](https://jmw86069.github.io/jamba/reference/formatInt.md),
[`gsubOrdered()`](https://jmw86069.github.io/jamba/reference/gsubOrdered.md),
[`gsubs()`](https://jmw86069.github.io/jamba/reference/gsubs.md),
[`makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md),
[`nameVector()`](https://jmw86069.github.io/jamba/reference/nameVector.md),
[`nameVectorN()`](https://jmw86069.github.io/jamba/reference/nameVectorN.md),
[`padInteger()`](https://jmw86069.github.io/jamba/reference/padInteger.md),
[`padString()`](https://jmw86069.github.io/jamba/reference/padString.md),
[`pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md),
[`pasteByRowOrdered()`](https://jmw86069.github.io/jamba/reference/pasteByRowOrdered.md),
[`sizeAsNum()`](https://jmw86069.github.io/jamba/reference/sizeAsNum.md),
[`tcount()`](https://jmw86069.github.io/jamba/reference/tcount.md),
[`ucfirst()`](https://jmw86069.github.io/jamba/reference/ucfirst.md)

## Examples

``` r
x <- c("A", "", "", "", "B", "C", "", "", NA,
   "D", "", "", "E", "F", "G", "", "");
data.frame(x, fillBlanks(x));
#>       x fillBlanks.x.
#> 1     A             A
#> 2                   A
#> 3                   A
#> 4                   A
#> 5     B             B
#> 6     C             C
#> 7                   C
#> 8                   C
#> 9  <NA>          <NA>
#> 10    D             D
#> 11                  D
#> 12                  D
#> 13    E             E
#> 14    F             F
#> 15    G             G
#> 16                  G
#> 17                  G
```
