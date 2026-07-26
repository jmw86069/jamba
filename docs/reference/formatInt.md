# Format an integer as a string

Format an integer as a string

## Usage

``` r
formatInt(
  x,
  big.mark = ",",
  trim = TRUE,
  forceInteger = TRUE,
  scientific = FALSE,
  ...
)
```

## Arguments

- x:

  `numeric` vector or matrix

- big.mark, trim, scientific:

  passed to [`base::format()`](https://rdrr.io/r/base/format.html) but
  configured with defaults intended for integer values:

  - `big.mark=","` adds comma between thousands.

  - `trim=TRUE` to trim excess whitespace.

  - `scientific=FALSE` to prevent exponential notation.

- forceInteger:

  `logical`, default TRUE, whether to round `numeric` to `integer` prior
  to calling [`base::format()`](https://rdrr.io/r/base/format.html).

- ...:

  Additional arguments are ignored.

## Value

`character` vector if `x` is a vector, or if `x` is a matrix a matrix
will be returned.

## Details

This function is a quick wrapper function around
[`base::format()`](https://rdrr.io/r/base/format.html) to display
integer values as text strings. It will also return a matrix if the
input is a matrix.

## See also

Other jam string functions:
[`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md),
[`breaksByVector()`](https://jmw86069.github.io/jamba/reference/breaksByVector.md),
[`fillBlanks()`](https://jmw86069.github.io/jamba/reference/fillBlanks.md),
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
x <- c(1234, 1234.56, 1234567.89);
## By default, commas are used for big.mark, and decimal values are hidden
formatInt(x);
#> [1] "1,234"     "1,235"     "1,234,568"

## By default, commas are used for big.mark
formatInt(x, forceInteger=FALSE);
#> [1] "1,234.00"     "1,234.56"     "1,234,567.89"
```
