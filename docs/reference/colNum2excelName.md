# convert column number to 'Excel' column name

convert column number to 'Excel' column name

## Usage

``` r
colNum2excelName(x, useLetters = LETTERS, zeroVal = "a", ...)
```

## Arguments

- x:

  `integer` vector

- useLetters:

  `character` vector of single-digit characters to use as digits in the
  resulting column name. Note that these characters can be of almost any
  length, with any content.

- zeroVal:

  `character` single-digit to be used whenever `x==0`, or as a prefix
  for negative values. In theory there should be no negative input
  values, but this basic mechanism is used to handle the possibility.

- ...:

  Additional arguments are ignored.

## Value

`character` vector with length(x)

## Details

The purpose is to convert an `integer` column number into a valid
'Excel' column name, using `LETTERS` starting at A. This function
implements an arbitrary number of digits, which may or may not be
compatible with each version of 'Excel'. 18,278 columns would be the
maximum for three digits, "A" through "ZZZ".

This function is useful when referencing 'Excel' columns via another
interface such as via openxlsx. It is also used by
[`makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md)
when the `numberStyle="letters"`, in order to provide letter suffix
values.

One can somewhat manipulate the allowed column names via the
`useLetters` argument, which by default uses the entire 26-letter
Western alphabet.

## See also

Other jam practical functions:
[`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md),
[`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md),
[`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md),
[`check_pkg_installed()`](https://jmw86069.github.io/jamba/reference/check_pkg_installed.md),
[`color_dither()`](https://jmw86069.github.io/jamba/reference/color_dither.md),
[`exp2signed()`](https://jmw86069.github.io/jamba/reference/exp2signed.md),
[`getAxisLabel()`](https://jmw86069.github.io/jamba/reference/getAxisLabel.md),
[`isFALSEV()`](https://jmw86069.github.io/jamba/reference/isFALSEV.md),
[`isTRUEV()`](https://jmw86069.github.io/jamba/reference/isTRUEV.md),
[`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md),
[`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md),
[`lldf()`](https://jmw86069.github.io/jamba/reference/lldf.md),
[`log2signed()`](https://jmw86069.github.io/jamba/reference/log2signed.md),
[`middle()`](https://jmw86069.github.io/jamba/reference/middle.md),
[`minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md),
[`newestFile()`](https://jmw86069.github.io/jamba/reference/newestFile.md),
[`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md),
[`reload_qmd_cache()`](https://jmw86069.github.io/jamba/reference/reload_qmd_cache.md),
[`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md),
[`renameColumn()`](https://jmw86069.github.io/jamba/reference/renameColumn.md),
[`rmInfinite()`](https://jmw86069.github.io/jamba/reference/rmInfinite.md),
[`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md),
[`rmNAs()`](https://jmw86069.github.io/jamba/reference/rmNAs.md),
[`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md),
[`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)

## Examples

``` r
colNum2excelName(1:30)
#>  [1] "A"  "B"  "C"  "D"  "E"  "F"  "G"  "H"  "I"  "J"  "K"  "L"  "M"  "N"  "O" 
#> [16] "P"  "Q"  "R"  "S"  "T"  "U"  "V"  "W"  "X"  "Y"  "Z"  "AA" "AB" "AC" "AD"
```
