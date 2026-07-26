# paste a list into a delimited vector

Paste a list of vectors into a character vector, with values delimited
by default with a comma.

## Usage

``` r
cPaste(
  x,
  sep = ",",
  doSort = FALSE,
  makeUnique = FALSE,
  na.rm = FALSE,
  keepFactors = FALSE,
  checkClass = TRUE,
  useBioc = TRUE,
  useLegacy = FALSE,
  honorFactor = TRUE,
  verbose = FALSE,
  ...
)

cPasteS(
  x,
  sep = ",",
  doSort = TRUE,
  makeUnique = FALSE,
  na.rm = FALSE,
  keepFactors = FALSE,
  checkClass = TRUE,
  useBioc = TRUE,
  ...
)

cPasteSU(
  x,
  sep = ",",
  doSort = TRUE,
  makeUnique = TRUE,
  na.rm = FALSE,
  keepFactors = FALSE,
  checkClass = TRUE,
  useBioc = TRUE,
  ...
)

cPasteUnique(
  x,
  sep = ",",
  doSort = FALSE,
  makeUnique = TRUE,
  na.rm = FALSE,
  keepFactors = FALSE,
  checkClass = TRUE,
  useBioc = TRUE,
  ...
)

cPasteU(
  x,
  sep = ",",
  doSort = FALSE,
  makeUnique = TRUE,
  na.rm = FALSE,
  keepFactors = FALSE,
  checkClass = TRUE,
  useBioc = TRUE,
  ...
)
```

## Arguments

- x:

  `list` of vectors

- sep:

  `character` delimiter used to paste multiple values together

- doSort:

  `logical` indicating whether to sort each vector using
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md).

- makeUnique:

  `logical` indicating whether to make each vector in the input list
  unique before pasting its values together.

- na.rm:

  `logical` indicating whether to remove NA values from each vector in
  the input list. When `na.rm` is `TRUE` and a list element contains
  only `NA` values, the resulting string will be `""`.

- keepFactors:

  `logical` only used when `useLegacy=TRUE` and `doSort=TRUE`;
  indicating whether to preserve factors, keeping factor level order.
  When `keepFactors=TRUE`, if any list element is a `factor`, all
  elements are converted to factors. Note that this step combines
  overall factor levels, and non-factors will be ordered using
  [`base::order()`](https://rdrr.io/r/base/order.html) instead of
  [`jamba::mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md)
  (for now.)

- checkClass:

  `logical`, default TRUE, whether to check the class of each vector in
  the input list.

  - When TRUE, it confirms the class of each element in the `list`
    before processing, to prevent conversion which may otherwise lose
    information.

  - For all cases when a known vector is split into a `list`,
    `checkClass=FALSE` is preferred since there is only one class in the
    resulting `list` elements. This approach is faster especially for
    for large input lists, 10000 or more.

  - When `checkClass=FALSE` it assumes all entries can be coerced to
    `character`, which is fastest, but does not preserve factor levels
    due to R coersion methods used by
    [`unlist()`](https://rdrr.io/r/base/unlist.html).

- useBioc:

  `logical` indicating whether this function should try to use
  [`S4Vectors::unstrsplit()`](https://rdrr.io/pkg/S4Vectors/man/character-utils.html)
  when the Bioconductor package `S4Vectors` is installed, otherwise it
  will use a less efficient
  [`mapply()`](https://rdrr.io/r/base/mapply.html) operation.

- useLegacy:

  `logical` indicating whether to enable to previous legacy process used
  by `cPaste()`.

- honorFactor:

  `logical` passed to
  [`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md),
  whether any `factor` vector should be sorted in factor level order.
  When `honorFactor=FALSE` then even `factor` vectors are sorted as if
  they were `character` vectors, ignoring the factor levels.

- verbose:

  `logical` indicating whether to print verbose output.

- ...:

  additional arguments are passed to
  [`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md)
  when `doSort=TRUE`.

## Value

`character` vector with the same names and in the same order as the
input list `x`.

## Details

- `cPaste()` concatenates vector values using a delimiter.

- `cPasteS()` sorts each vector using
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md).

- `cPasteU()` applies
  [`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md)
  to retain unique values per vector.

- `cPasteSU()` applies
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  and
  [`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md).

This function is essentially a wrapper for
[`S4Vectors::unstrsplit()`](https://rdrr.io/pkg/S4Vectors/man/character-utils.html)
except that it also optionally applies uniqueness to each vector in the
list, and sorts values in each vector using
[`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md).

The sorting and uniqueness is applied to the `unlist`ed vector of
values, which is substantially faster than any `apply` family function
equivalent. The uniqueness is performed by
[`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md),
which itself will use `S4Vectors::unique()` if available.

## See also

Other jam list functions:
[`heads()`](https://jmw86069.github.io/jamba/reference/heads.md),
[`jam_rapply()`](https://jmw86069.github.io/jamba/reference/jam_rapply.md),
[`list2df()`](https://jmw86069.github.io/jamba/reference/list2df.md),
[`mergeAllXY()`](https://jmw86069.github.io/jamba/reference/mergeAllXY.md),
[`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md),
[`rbindList()`](https://jmw86069.github.io/jamba/reference/rbindList.md),
[`relist_named()`](https://jmw86069.github.io/jamba/reference/relist_named.md),
[`rlengths()`](https://jmw86069.github.io/jamba/reference/rlengths.md),
[`sclass()`](https://jmw86069.github.io/jamba/reference/sclass.md),
[`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md),
[`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md),
[`unnestList()`](https://jmw86069.github.io/jamba/reference/unnestList.md)

## Examples

``` r
L1 <- list(CA=LETTERS[c(1:4,2,7,4,6)], B=letters[c(7:11,9,3)]);

cPaste(L1);
#>                CA                 B 
#> "A,B,C,D,B,G,D,F"   "g,h,i,j,k,i,c" 
#               CA                 B
# "A,B,C,D,B,G,D,F"   "g,h,i,j,k,i,c"

cPaste(L1, doSort=TRUE);
#>                CA                 B 
#> "A,B,B,C,D,D,F,G"   "c,g,h,i,i,j,k" 
#               CA                 B
# "A,B,B,C,D,D,F,G"   "c,g,h,i,i,j,k"

## The sort can be done with convenience function cPasteS()
cPasteS(L1);
#>                CA                 B 
#> "A,B,B,C,D,D,F,G"   "c,g,h,i,i,j,k" 
#               CA                 B
# "A,B,B,C,D,D,F,G"   "c,g,h,i,i,j,k"

## Similarly, makeUnique=TRUE and cPasteU() are the same
cPaste(L1, makeUnique=TRUE);
#>            CA             B 
#> "A,B,C,D,G,F" "g,h,i,j,k,c" 
cPasteU(L1);
#>            CA             B 
#> "A,B,C,D,G,F" "g,h,i,j,k,c" 
#           CA             B
# "A,B,C,D,G,F" "g,h,i,j,k,c"

## Change the delimiter
cPasteSU(L1, sep="; ")
#>                 CA                  B 
#> "A; B; C; D; F; G" "c; g; h; i; j; k" 
#                CA                  B
# "A; B; C; D; F; G" "c; g; h; i; j; k"

# test mix of factor and non-factor
L2 <- c(
   list(D=factor(letters[1:12],
      levels=letters[12:1])),
   L1);
L2;
#> $D
#>  [1] a b c d e f g h i j k l
#> Levels: l k j i h g f e d c b a
#> 
#> $CA
#> [1] "A" "B" "C" "D" "B" "G" "D" "F"
#> 
#> $B
#> [1] "g" "h" "i" "j" "k" "i" "c"
#> 
cPasteSU(L2, keepFactors=TRUE);
#>                         D                        CA                         B 
#> "l,k,j,i,h,g,f,e,d,c,b,a"             "A,B,C,D,F,G"             "c,g,h,i,j,k" 

# tricky example with mix of character and factor
# and factor levels are inconsistent
# end result: factor levels are defined in order they appear
L <- list(entryA=c("miR-112", "miR-12", "miR-112"),
   entryB=factor(c("A","B","A","B"),
      levels=c("B","A")),
   entryC=factor(c("C","A","B","B","C"),
      levels=c("A","B","C")),
   entryNULL=NULL)
L;
#> $entryA
#> [1] "miR-112" "miR-12"  "miR-112"
#> 
#> $entryB
#> [1] A B A B
#> Levels: B A
#> 
#> $entryC
#> [1] C A B B C
#> Levels: A B C
#> 
#> $entryNULL
#> NULL
#> 
cPaste(L);
#>                   entryA                   entryB                   entryC 
#> "miR-112,miR-12,miR-112"                "A,B,A,B"              "C,A,B,B,C" 
#>                entryNULL 
#>                       "" 
cPasteU(L);
#>           entryA           entryB           entryC        entryNULL 
#> "miR-112,miR-12"            "A,B"          "C,A,B"               "" 

# by default keepFactors=FALSE, which means factors are sorted as characters
cPasteS(L);
#>                   entryA                   entryB                   entryC 
#> "miR-12,miR-112,miR-112"                "B,B,A,A"              "B,B,A,C,C" 
#>                entryNULL 
#>                       "" 
cPasteSU(L);
#>           entryA           entryB           entryC        entryNULL 
#> "miR-12,miR-112"            "B,A"          "B,A,C"               "" 
# keepFactors=TRUE will keep unique factor levels in the order they appear
# this is the same behavior as unlist(L[c(2,3)]) on a list of factors
cPasteSU(L, keepFactors=TRUE);
#>           entryA           entryB           entryC        entryNULL 
#> "miR-12,miR-112"            "B,A"          "B,A,C"               "" 
levels(unlist(L[c(2,3)]))
#> [1] "B" "A" "C"
```
