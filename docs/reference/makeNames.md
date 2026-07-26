# make unique vector names

make unique vector names

## Usage

``` r
makeNames(
  x,
  unique = TRUE,
  suffix = "_v",
  renameOnes = FALSE,
  doPadInteger = FALSE,
  startN = 1,
  numberStyle = c("number", "letters", "LETTERS"),
  useNchar = NULL,
  renameFirst = TRUE,
  keepNA = TRUE,
  ...
)
```

## Arguments

- x:

  character vector to be used when defining names. All other vector
  types will be coerced to character prior to use.

- unique:

  argument which is ignored, included only for compatibility with
  [`base::make.names`](https://rdrr.io/r/base/make.names.html). All
  results from `makeNames()` are unique.

- suffix:

  character separator between the original entry and the version, if
  necessary.

- renameOnes:

  logical whether to rename single, unduplicated, entries.

- doPadInteger:

  logical whether to pad integer values to a consistent number of
  digits, based upon all suffix values needed. This output allows for
  more consistent sorting of names. To define a fixed number of digits,
  use the useNchar parameter.

- startN:

  integer number used when numberStyle is "number", this integer is used
  for the first entry to be renamed. You can use this value to make
  zero-based suffix values, for example.

- numberStyle:

  character style for version numbering

  "number"

  :   Use integer numbers to represent each duplicated entry.

  "letters"

  :   Use lowercase letters to represent each duplicated entry. The 27th
      entry uses the pattern "aa" to represent two 26-base digits. When
      doPadInteger=TRUE, a zero is still used to pad the resulting
      version numbers, again to allow easy sorting of text values, but
      also because there is no letter equivalent for the number zero. It
      is usually best to change the suffix to "\_" or "" when using
      "letters".

  "LETTERS"

  :   Use uppercase letters to represent each duplicated entry, with the
      same rules as applied to "letters".

- useNchar:

  integer or NULL, number of digits to use when padding integer values
  with leading zero, only relevant when usePadInteger=TRUE.

- renameFirst:

  logical whether to rename the first entry in a set of duplicated
  entries. If FALSE then the first entry in a set will not be versioned,
  even when renameOnes=TRUE.

- keepNA:

  logical whether to retain NA values using the string "NA". If keepNA
  is FALSE, then NA values will remain NA, thus causing some names to
  become `<NA>`, which can cause problems with some downstream functions
  which assume all names are either NULL or non-NA.

- ...:

  Additional arguments are ignored.

## Value

character vector of unique names

## Details

This function extends the basic goal from
[`make.names`](https://rdrr.io/r/base/make.names.html) which is intended
to make syntactically valid names from a character vector. This
makeNames function makes names unique, and offers configurable methods
to handle duplicate names. By default, any duplicated entries receive a
suffix \_v# where \# is s running count of entries observed, starting
at 1. The [`make.names`](https://rdrr.io/r/base/make.names.html)
function, by contrast, renames the second observed entry starting at .1,
leaving the original entry unchanged. Optionally, makeNames can rename
all entries with a numeric suffix, for consistency.

For example: `A, A, A, B, B, C` becomes:
`A_v1, A_v2, A_v3, B_v1, B_v2, C`

Also, makeNames always allows "\_".

This makeNames function is similar to
[`make.unique`](https://rdrr.io/r/base/make.unique.html) which also
converts a vector into a unique vector by adding suffix values, however
the [`make.unique`](https://rdrr.io/r/base/make.unique.html) function
intends to allow repeated operations which recognize duplicated entries
and continually increment the suffix number. This makeNames function
currently does not handle repeat operations. The recommended approach to
workaround having pre-existing versioned names would be to remove suffix
values prior to running this function. One small distinction from
[`make.unique`](https://rdrr.io/r/base/make.unique.html) is that
makeNames does version the first entry in a set.

## See also

Other jam string functions:
[`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md),
[`breaksByVector()`](https://jmw86069.github.io/jamba/reference/breaksByVector.md),
[`fillBlanks()`](https://jmw86069.github.io/jamba/reference/fillBlanks.md),
[`formatInt()`](https://jmw86069.github.io/jamba/reference/formatInt.md),
[`gsubOrdered()`](https://jmw86069.github.io/jamba/reference/gsubOrdered.md),
[`gsubs()`](https://jmw86069.github.io/jamba/reference/gsubs.md),
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
V <- rep(LETTERS[1:3], c(2,3,1));
makeNames(V);
#> [1] "A_v1" "A_v2" "B_v1" "B_v2" "B_v3" "C"   
makeNames(V, renameOnes=TRUE);
#> [1] "A_v1" "A_v2" "B_v1" "B_v2" "B_v3" "C_v1"
makeNames(V, renameFirst=FALSE);
#> [1] "A"    "A_v1" "B"    "B_v1" "B_v2" "C"   
exons <- makeNames(rep("exon", 3), suffix="");
makeNames(rep(exons, c(2,3,1)), numberStyle="letters", suffix="");
#> [1] "exon1a" "exon1b" "exon2a" "exon2b" "exon2c" "exon3" 
```
