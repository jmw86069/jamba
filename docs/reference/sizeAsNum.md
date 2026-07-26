# convert size to numeric value

convert size to numeric value

## Usage

``` r
sizeAsNum(x, kiloSize = 1024, verbose = FALSE, ...)
```

## Arguments

- x:

  `character` vector. When `x` is numeric, it is returned as-is;
  otherwise x is coerced to `character` with
  [`as.character()`](https://rdrr.io/r/base/character.html) and will
  throw an error if it fails.

- kiloSize:

  `numeric` number of base units when converting from one base unit, to
  one "kilo" base unit. For file sizes, this value is 1024, but for
  other purposes this value may be 1000, like one thousand units is
  `"1k units"`.

- verbose:

  `logical` indicating whether to print verbose output. The output
  includes a `data.frame` summarizing the input, and the unit matched,
  and the final value. If `verbose==2` it will return this `data.frame`
  for review.

- ...:

  additional arguments are ignored.

## Value

`numeric` vector representing the numeric value represented by an
abbreviated size.

## Details

This function is intended to provide the inverse of
[`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md) by
converting an abbreviated size into a full numeric value.

It makes one simplifying assumption, that the first character in the
unit is enough to determine the unit. This assumption also means the
units are currently case-sensitive, for example `Mega` requires
upper-case `"M"`, because `"milli"` which is not supported, requires
`"m"`.

Unit abbreviations recognized:

- `k` - kilo - size is defined by `kiloSize`

- `M` - Mega - size is defined by `kiloSize ^ 2`

- `G` - Giga - size is defined by `kiloSize ^ 3`

- `T` - Tera - size is defined by `kiloSize ^ 4`

- `P` - Peta - size is defined by `kiloSize ^ 5`

Everything else is considered to have no abbreviated units, thus the
numeric value is returned as-is.

Note that the round trip
[`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md)
followed by `sizeAsNum()` will not produce identical values, because the
intermediate value is rounded by `digits` in
[`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md).

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
[`padString()`](https://jmw86069.github.io/jamba/reference/padString.md),
[`pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md),
[`pasteByRowOrdered()`](https://jmw86069.github.io/jamba/reference/pasteByRowOrdered.md),
[`tcount()`](https://jmw86069.github.io/jamba/reference/tcount.md),
[`ucfirst()`](https://jmw86069.github.io/jamba/reference/ucfirst.md)

## Examples

``` r
x <- asSize(c(1, 10,2010,22000,52200), unitType="")
x
#> [1] "1"      "10"     "1.96 k" "21.5 k" "51 k"  
#> "1"   "10" "2k"     "21k"    "51k"
sizeAsNum(x)
#> [1]     1.00    10.00  2007.04 22016.00 52224.00

sizeAsNum(x, kiloSize=1000)
#> [1]     1    10  1960 21500 51000
```
