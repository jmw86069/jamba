# Pattern replacement with multiple patterns

Pattern replacement with multiple patterns

## Usage

``` r
gsubs(
  pattern,
  replacement,
  x,
  ignore.case = TRUE,
  replaceMultiple = rep(TRUE, length(pattern)),
  ...
)
```

## Arguments

- pattern:

  `character` vector of patterns

- replacement:

  `character` vector of replacements

- x:

  `character` vector with input data to be curated

- ignore.case:

  `logical` indicating whether to perform pattern matching in
  case-insensitive manner, where `ignore.case=TRUE` will ignore the
  uppercase/lowercase distinction.

- replaceMultiple:

  `logical` vector indicating whether to perform global substitution,
  where `replaceMultiple=FALSE` will only replace the first occurrence
  of the pattern, using
  [`base::sub()`](https://rdrr.io/r/base/grep.html). Note that this
  vector can refer to individual entries in `pattern`.

- ...:

  additional arguments are passed to
  [`base::gsub()`](https://rdrr.io/r/base/grep.html) or
  [`base::sub()`](https://rdrr.io/r/base/grep.html).

## Value

`character` vector when input `x` is an atomic vector, or `list` when
input `x` is a `list`.

## Details

This function is a simple wrapper around
[`base::gsub()`](https://rdrr.io/r/base/grep.html) when considering a
series of pattern-replacement combinations. It applies each pattern
match and replacement in order and is therefore not vectorized.

When `x` input is a `list` each vector in the `list` is processed,
somewhat differently than processing one vector.

1.  When the `list` contains another `list`, or when `length(x) < 100`,
    each value in `x` is iterated calling `gsubs()`. This process is the
    slowest option, however not noticeble until `x` has length over
    10,000.

2.  When the `list` does not contain another `list` and all values are
    non-factor, or all values are `factor`, they are unlisted, processed
    as a vector, then relisted. This process is nearly the same speed as
    processing one single vector, except the time it takes to confirm
    the list element classes.

3.  When values contain a mix of non-factor and `factor` values, they
    are separately unlisted, processed by `gsubs()`, then relisted and
    combined afterward. Again, this process is only slightly slower than
    option 2 above, given that it calls `gsubs()` twice, with two
    vectors.

4.  Note that `factor` values at input are replaced with `character`
    values at output, consistent with
    [`gsub()`](https://rdrr.io/r/base/grep.html).

## See also

Other jam string functions:
[`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md),
[`breaksByVector()`](https://jmw86069.github.io/jamba/reference/breaksByVector.md),
[`fillBlanks()`](https://jmw86069.github.io/jamba/reference/fillBlanks.md),
[`formatInt()`](https://jmw86069.github.io/jamba/reference/formatInt.md),
[`gsubOrdered()`](https://jmw86069.github.io/jamba/reference/gsubOrdered.md),
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
gsubs(c("one", "two"), c("three", "four"), "one two five six")
#> [1] "three four five six"
gsubs(c("one", "two"), c("three"), "one two five six")
#> [1] "three three five six"
```
