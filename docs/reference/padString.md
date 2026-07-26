# pad a character string to a fixed length

pad a character string to a fixed length

## Usage

``` r
padString(
  x,
  stringLength = max(nchar(x)),
  padCharacter = " ",
  justify = "left",
  ...
)
```

## Arguments

- x:

  `character` vector

- stringLength:

  `integer` length for the resulting character strings in `x`. By
  default, all strings are padded to the length of the longest entry,
  however stringLength can be defined to impose strict number of
  characters for all entries.

- padCharacter:

  `character` string with nchar=1 used for padding.

- justify:

  `character` string with "left", "right", "center" to indicate
  alignment of the resulting text string.

- ...:

  additional parameters are ignored.

## Value

`character` vector of length(x)

## See also

Other jam string functions:
[`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md),
[`breaksByVector()`](https://jmw86069.github.io/jamba/reference/breaksByVector.md),
[`fillBlanks()`](https://jmw86069.github.io/jamba/reference/fillBlanks.md),
[`formatInt()`](https://jmw86069.github.io/jamba/reference/formatInt.md),
[`gsubOrdered()`](https://jmw86069.github.io/jamba/reference/gsubOrdered.md),
[`gsubs()`](https://jmw86069.github.io/jamba/reference/gsubs.md),
[`makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md),
[`nameVector()`](https://jmw86069.github.io/jamba/reference/nameVector.md),
[`nameVectorN()`](https://jmw86069.github.io/jamba/reference/nameVectorN.md),
[`padInteger()`](https://jmw86069.github.io/jamba/reference/padInteger.md),
[`pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md),
[`pasteByRowOrdered()`](https://jmw86069.github.io/jamba/reference/pasteByRowOrdered.md),
[`sizeAsNum()`](https://jmw86069.github.io/jamba/reference/sizeAsNum.md),
[`tcount()`](https://jmw86069.github.io/jamba/reference/tcount.md),
[`ucfirst()`](https://jmw86069.github.io/jamba/reference/ucfirst.md)

## Examples

``` r
padString(c("one","two","three"));
#>     one     two   three 
#> "one  " "two  " "three" 
padString(c("one","two","three","four"), padCharacter="_", justify="center");
#>     one     two   three    four 
#> "_one_" "_two_" "three" "_four" 
```
