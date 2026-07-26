# Uppercase the first letter in each word

Uppercase the first letter in each word

## Usage

``` r
ucfirst(x, lowercaseAll = FALSE, firstWordOnly = FALSE, ...)
```

## Arguments

- x:

  character vector.

- lowercaseAll:

  logical indicating whether to force all letters to lowercase before
  applying uppercase to the first letter.

- firstWordOnly:

  logical indicating whether to apply the uppercase only to the first
  word in each string. Note that it still applies the logic to every
  entry in the input vector `x`.

- ...:

  additional arguments are ignored.

## Value

`character` vector where letters are converted to uppercase.

## Details

This function is a simple mimic of the Perl function `ucfirst` which
converts the first letter in each word to uppercase. When
`lowercaseAll=TRUE` it also forces all other letters to lowercase,
otherwise mixedCase words will retain capital letters in the middle of
words.

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
[`sizeAsNum()`](https://jmw86069.github.io/jamba/reference/sizeAsNum.md),
[`tcount()`](https://jmw86069.github.io/jamba/reference/tcount.md)

## Examples

``` r
ucfirst("TESTING_ALL_UPPERCASE_INPUT")
#> [1] "TESTING_ALL_UPPERCASE_INPUT"
ucfirst("TESTING_ALL_UPPERCASE_INPUT", TRUE)
#> [1] "Testing_All_Uppercase_Input"
ucfirst("TESTING_ALL_UPPERCASE_INPUT", TRUE, TRUE)
#> [1] "Testing_all_uppercase_input"

ucfirst("testing mixedCase upperAndLower case input")
#> [1] "Testing MixedCase UpperAndLower Case Input"
ucfirst("testing mixedCase upperAndLower case input", TRUE)
#> [1] "Testing Mixedcase Upperandlower Case Input"
ucfirst("testing mixedCase upperAndLower case input", TRUE, TRUE)
#> [1] "Testing mixedcase upperandlower case input"
```
