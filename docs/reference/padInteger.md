# prefix integers with leading zeros

prefix integers with leading zeros

## Usage

``` r
padInteger(x, padCharacter = "0", useNchar = NULL, ...)
```

## Arguments

- x:

  `integer`, `numeric`, or `character` vector. In reality, only
  `nchar(x)` is required to determine padding.

- padCharacter:

  `character` with nchar(padCharacter)==1, used to pad each digit as a
  prefix.

- useNchar:

  'NULL' or `integer` number of digits used, or if the maximum
  `nchar(x)` is higher, that number of digits is used. Note `useNchar`
  is mostly useful when all numbers are less than 10, but the desired
  output is to have a fixed number of digits 2 or higher.

- ...:

  additional parameters are ignored.

## Value

`character` vector of length(x).

## Details

The purpose of this function is to pad integer numbers so they contain a
consistent number of digits, which is helpful when sorting values as
character strings.

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
[`padString()`](https://jmw86069.github.io/jamba/reference/padString.md),
[`pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md),
[`pasteByRowOrdered()`](https://jmw86069.github.io/jamba/reference/pasteByRowOrdered.md),
[`sizeAsNum()`](https://jmw86069.github.io/jamba/reference/sizeAsNum.md),
[`tcount()`](https://jmw86069.github.io/jamba/reference/tcount.md),
[`ucfirst()`](https://jmw86069.github.io/jamba/reference/ucfirst.md)

## Examples

``` r
padInteger(c(1, 10, 20, 300, 5000))
#> [1] "0001" "0010" "0020" "0300" "5000"
```
