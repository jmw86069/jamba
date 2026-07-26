# Global substitution into ordered factor

Global substitution into ordered factor

## Usage

``` r
gsubOrdered(
  pattern,
  replacement,
  x,
  ignore.case = FALSE,
  perl = FALSE,
  fixed = FALSE,
  useBytes = FALSE,
  sortFunc = mixedSort,
  ...
)
```

## Arguments

- pattern, replacement, x, ignore.case, perl, fixed, useBytes:

  arguments sent to [`base::gsub()`](https://rdrr.io/r/base/grep.html)

- sortFunc:

  function used to sort factor levels, which is not performed if the
  input `x` is a `factor`.

- ...:

  additional arguments are passed to `sortFunc`

## Value

factor whose levels are based upon the order of input levels when the
input `x` is a factor; or if the input `x` is not a factor, it is
converted to a factor using the provided sort function `sortFunc`.

## Details

This function is an extension of
[`base::gsub()`](https://rdrr.io/r/base/grep.html) that returns an
ordered factor output. When input is also a factor, the output factor
levels are retained in the same order, after applying the string
substitution.

This function is very useful when making changes via
[`base::gsub()`](https://rdrr.io/r/base/grep.html) to a factor with
ordered levels, because it retains the the order of levels after
modification.

Tips:

- To convert a character vector to a factor, whose levels are sorted,
  use `sortFunc=sort`.

- To convert a character vector to a factor, whose levels are the order
  they appear in the input `x`, use `sortFunc=c`.

- To convert a character vector to a factor, whose levels are sorted
  alphanumerically, use `sortFunc=mixedSort`.

## See also

Other jam string functions:
[`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md),
[`breaksByVector()`](https://jmw86069.github.io/jamba/reference/breaksByVector.md),
[`fillBlanks()`](https://jmw86069.github.io/jamba/reference/fillBlanks.md),
[`formatInt()`](https://jmw86069.github.io/jamba/reference/formatInt.md),
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
x <- c(paste0(
   rep(c("first", "second", "third"), 2),
   rep(c("Section", "Choice"), each=3)),
   "Choice");
f <- factor(x, levels=x);
f;
#> [1] firstSection  secondSection thirdSection  firstChoice   secondChoice 
#> [6] thirdChoice   Choice       
#> 7 Levels: firstSection secondSection thirdSection firstChoice ... Choice

# default gsub() will return a character vector
gsub("(first|second|third)", "", f)
#> [1] "Section" "Section" "Section" "Choice"  "Choice"  "Choice"  "Choice" 
# converting to factor resets the factor level order
factor(gsub("(first|second|third)", "", f))
#> [1] Section Section Section Choice  Choice  Choice  Choice 
#> Levels: Choice Section

## gsubOrdered() maintains the factor level order
gsubOrdered("(first|third)", "", f)
#> [1] Section       secondSection Section       Choice        secondChoice 
#> [6] Choice        Choice       
#> Levels: Section secondSection Choice secondChoice
gsubOrdered("(first)", "", f)
#> [1] Section       secondSection thirdSection  Choice        secondChoice 
#> [6] thirdChoice   Choice       
#> 6 Levels: Section secondSection thirdSection Choice ... thirdChoice

# to convert character vector to factor, levels in order they appear
gsubOrdered("", "", x, sortFunc=c)
#> [1] firstSection  secondSection thirdSection  firstChoice   secondChoice 
#> [6] thirdChoice   Choice       
#> 7 Levels: firstSection secondSection thirdSection firstChoice ... Choice

# to convert character vector to factor, levels alphanumeric sorted
gsubOrdered("", "", x, sortFunc=mixedSort)
#> [1] firstSection  secondSection thirdSection  firstChoice   secondChoice 
#> [6] thirdChoice   Choice       
#> 7 Levels: Choice firstChoice firstSection secondChoice ... thirdSection
```
