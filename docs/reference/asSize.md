# convert numeric value or R object to human-readable size

convert numeric value or R object to human-readable size

## Usage

``` r
asSize(
  x,
  digits = 3,
  abbreviateUnits = TRUE,
  unitType = "bytes",
  unitAbbrev = gsub("^(.).*$", "\\1", unitType),
  kiloSize = 1024,
  sep = " ",
  ...
)
```

## Arguments

- x:

  `numeric` vector, class `object_size` which is converted to `numeric`,
  any other R object is converted to a single `numeric` value using
  [`utils::object.size()`](https://rdrr.io/r/utils/object.size.html).

- digits:

  `integer` number of digits used by
  [`base::format()`](https://rdrr.io/r/base/format.html) when formatting
  the number to create a character string

- abbreviateUnits:

  `logical`, default TRUE, whether to print abbreviated units, for
  example using k, M, G, T, P instead of kilo, mega, Giga, Tera, Peta,
  respectively.

- unitType:

  `character` string indicating the base unit of measure, by default
  "bytes". Note that trailing "s" is removed when the number is
  singular.

- unitAbbrev:

  `character` string indicating an abbreviated base unit, by default it
  uses the first character from `unitType.`

- kiloSize:

  `numeric`, default 1024, number of base units when converting from to
  one "kilo" base unit. For computer-based size such as file size and
  object size, this value is 1024. For other purposes, such as
  scientific or monetary numbers, this value should usually be 1000.

- sep:

  `delimiter` used between the numeric value and the unit, default " ".

- ...:

  other parameters passed to
  [`base::format()`](https://rdrr.io/r/base/format.html).

## Value

`character` vector representing human-friendly size, based upon the
`kiloSize` argument to determine whether to report byte (1024) or
scientific (1000) units.

## Details

This function returns human-readable size based upon `numeric` input.
Alternatively, when input is any other R object, it calls
[`utils::object.size()`](https://rdrr.io/r/utils/object.size.html) to
produce a single `numeric` value which is then used to produce
human-readable size.

The default behavior is to report computer size in bytes, where 1024 is
considered "kilo", however argument `kiloSize` can be used to produce
values where `kiloSize=1000` which is suitable for monetary and other
scientific values.

## See also

Other jam string functions:
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
[`sizeAsNum()`](https://jmw86069.github.io/jamba/reference/sizeAsNum.md),
[`tcount()`](https://jmw86069.github.io/jamba/reference/tcount.md),
[`ucfirst()`](https://jmw86069.github.io/jamba/reference/ucfirst.md)

## Examples

``` r
asSize(c(1, 10,2010,22000,52200))
#> [1] "1 byte"   "10 bytes" "1.96 kb"  "21.5 kb"  "51 kb"   
#> "1 byte"   "10 bytes" "2 kb"     "21 kb"    "51 kb"

# demonstration of straight numeric units
asSize(c(1, 100, 1000, 10000), unitType="", kiloSize=100)
#> [1] "1"    "1 k"  "10 k" "1 M" 
```
