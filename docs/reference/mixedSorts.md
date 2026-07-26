# sort alphanumeric values within a list format

sort alphanumeric values within a list format

## Usage

``` r
mixedSorts(
  x,
  blanksFirst = TRUE,
  na.last = NAlast,
  keepNegative = FALSE,
  keepInfinite = TRUE,
  keepDecimal = FALSE,
  ignore.case = TRUE,
  useCaseTiebreak = TRUE,
  sortByName = FALSE,
  na.rm = FALSE,
  verbose = FALSE,
  NAlast = TRUE,
  honorFactor = TRUE,
  xclass = NULL,
  indent = 0,
  debug = FALSE,
  ...
)
```

## Arguments

- x:

  `vector`

- blanksFirst:

  `logical` whether to order blank entries before entries containing a
  value.

- na.last:

  `logical` indicating whether to move NA entries at the end of the
  sort.

- keepNegative:

  `logical` whether to keep '-' associated with adjacent numeric values,
  in order to sort them as negative values.

- keepInfinite:

  `logical` whether to allow "Inf" to be considered a numeric infinite
  value.

- keepDecimal:

  `logical` whether to keep the decimal in numbers, sorting as a true
  number and not as a version number. By default keepDecimal=FALSE,
  which means "v1.200" should be ordered before "v1.30". When
  keepDecimal=TRUE, the numeric sort considers only "1.2" and "1.3" and
  sorts in that order.

- ignore.case:

  `logical` whether to ignore uppercase and lowercase characters when
  defining the sort order. Note that when `x` is `factor` the factor
  levels are converted using `unique(toupper(levels(x)))`, therefore the
  values in `x` will be sorted by factor level.

- useCaseTiebreak:

  `logical` indicating whether to break ties when `ignore.case=TRUE`,
  using mixed case as a tiebreaker.

- sortByName:

  `logical` whether to sort the vector x by names(x) instead of sorting
  by x itself.

- na.rm:

  `logical`, default FALSE, indicating whether to remove NA values.

- verbose:

  `logical` whether to print verbose output.

- NAlast:

  `logical` deprecated in favor of argument `na.last` for consistency
  with [`base::sort()`](https://rdrr.io/r/base/sort.html).

- honorFactor:

  `logical`, default TRUE, used to enforce factor level sort order, when
  FALSE it sorts as `character`.

- xclass:

  `character` vector of classes in `x`, used for slight optimization to
  re-use this vector if it has already been defined for `x`. When 'NULL'
  it is created within this function.

- indent:

  `numeric` used only when `verbose=TRUE` to determine the number of
  spaces indented for verbose output, passed to
  [`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md).

- debug:

  `logical`, default FALSE, whether to print detailed debug output.

- ...:

  additional parameters are sent to
  [`mixedOrder`](https://jmw86069.github.io/jamba/reference/mixedOrder.md).

## Value

`list` after applying
[`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
to its elements.

## Details

This function is an extension to
[`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
to sort each vector in a list. It applies the sort to the whole unlisted
vector then splits back into list form.

In the event the input is a nested list of lists, only the first level
of list structure is maintained in the output data. For more
information, see
[`rlengths()`](https://jmw86069.github.io/jamba/reference/rlengths.md)
which calculates the recursive nested list sizes. An exception is when
the data contained in `x` represents multiple classes, see below.

When data in `x` represents multiple classes, for example `character`
and `factor`, the mechanism is slightly different and not as well-
optimized for large length `x`. The method uses
`rapply(x, how="replace", mixedSort)` which recursively, and
iteratively, calls
[`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
on each vector, and therefore returns data in the same nested `list`
structure as provided in `x`.

When data in `x` represents only one class, data is
[`unlist()`](https://rdrr.io/r/base/unlist.html) to one large vector,
which is sorted with
[`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md),
then split back into `list` structure representing `x` input.

## See also

Other jam sort functions:
[`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md),
[`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md),
[`mixedSortDF()`](https://jmw86069.github.io/jamba/reference/mixedSortDF.md),
[`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md)

Other jam list functions:
[`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md),
[`heads()`](https://jmw86069.github.io/jamba/reference/heads.md),
[`jam_rapply()`](https://jmw86069.github.io/jamba/reference/jam_rapply.md),
[`list2df()`](https://jmw86069.github.io/jamba/reference/list2df.md),
[`mergeAllXY()`](https://jmw86069.github.io/jamba/reference/mergeAllXY.md),
[`rbindList()`](https://jmw86069.github.io/jamba/reference/rbindList.md),
[`relist_named()`](https://jmw86069.github.io/jamba/reference/relist_named.md),
[`rlengths()`](https://jmw86069.github.io/jamba/reference/rlengths.md),
[`sclass()`](https://jmw86069.github.io/jamba/reference/sclass.md),
[`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md),
[`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md),
[`unnestList()`](https://jmw86069.github.io/jamba/reference/unnestList.md)

## Examples

``` r
# set up an example list of mixed alpha-numeric strings
set.seed(12);
x <- paste0(sample(letters, replace=TRUE, 52), rep(1:30, length.out=52));
x;
#>  [1] "b1"  "z2"  "p3"  "w4"  "n5"  "e6"  "e7"  "b8"  "h9"  "r10" "r11" "y12"
#> [13] "f13" "l14" "j15" "z16" "g17" "p18" "m19" "x20" "d21" "x22" "y23" "h24"
#> [25] "z25" "k26" "n27" "g28" "l29" "e30" "y1"  "b2"  "g3"  "n4"  "g5"  "z6" 
#> [37] "t7"  "t8"  "s9"  "t10" "d11" "m12" "m13" "p14" "e15" "w16" "p17" "o18"
#> [49] "v19" "g20" "m21" "t22"
# split into a list as an example
xL <- split(x, rep(letters[1:5], c(6,7,5,4,4)));
xL;
#> $a
#>  [1] "b1"  "z2"  "p3"  "w4"  "n5"  "e6"  "n27" "g28" "l29" "e30" "y1"  "b2" 
#> 
#> $b
#>  [1] "e7"  "b8"  "h9"  "r10" "r11" "y12" "f13" "g3"  "n4"  "g5"  "z6"  "t7" 
#> [13] "t8"  "s9" 
#> 
#> $c
#>  [1] "l14" "j15" "z16" "g17" "p18" "t10" "d11" "m12" "m13" "p14"
#> 
#> $d
#> [1] "m19" "x20" "d21" "x22" "e15" "w16" "p17" "o18"
#> 
#> $e
#> [1] "y23" "h24" "z25" "k26" "v19" "g20" "m21" "t22"
#> 

# now run mixedSorts(xL)
# Notice "e6" is sorted before "e30"
mixedSorts(xL)
#> $a
#>  [1] "b1"  "b2"  "e6"  "e30" "g28" "l29" "n5"  "n27" "p3"  "w4"  "y1"  "z2" 
#> 
#> $b
#>  [1] "b8"  "e7"  "f13" "g3"  "g5"  "h9"  "n4"  "r10" "r11" "s9"  "t7"  "t8" 
#> [13] "y12" "z6" 
#> 
#> $c
#>  [1] "d11" "g17" "j15" "l14" "m12" "m13" "p14" "p18" "t10" "z16"
#> 
#> $d
#> [1] "d21" "e15" "m19" "o18" "p17" "w16" "x20" "x22"
#> 
#> $e
#> [1] "g20" "h24" "k26" "m21" "t22" "v19" "y23" "z25"
#> 

# for fun, compare to lapply(xL, sort)
# Notice "e6" is sorted after "e30"
lapply(xL, sort)
#> $a
#>  [1] "b1"  "b2"  "e30" "e6"  "g28" "l29" "n27" "n5"  "p3"  "w4"  "y1"  "z2" 
#> 
#> $b
#>  [1] "b8"  "e7"  "f13" "g3"  "g5"  "h9"  "n4"  "r10" "r11" "s9"  "t7"  "t8" 
#> [13] "y12" "z6" 
#> 
#> $c
#>  [1] "d11" "g17" "j15" "l14" "m12" "m13" "p14" "p18" "t10" "z16"
#> 
#> $d
#> [1] "d21" "e15" "m19" "o18" "p17" "w16" "x20" "x22"
#> 
#> $e
#> [1] "g20" "h24" "k26" "m21" "t22" "v19" "y23" "z25"
#> 

# test super-long list
xL10k <- rep(xL, length.out=10000);
names(xL10k) <- as.character(seq_along(xL10k));
print(head(mixedSorts(xL10k), 10))
#> $`1`
#>  [1] "b1"  "b2"  "e6"  "e30" "g28" "l29" "n5"  "n27" "p3"  "w4"  "y1"  "z2" 
#> 
#> $`2`
#>  [1] "b8"  "e7"  "f13" "g3"  "g5"  "h9"  "n4"  "r10" "r11" "s9"  "t7"  "t8" 
#> [13] "y12" "z6" 
#> 
#> $`3`
#>  [1] "d11" "g17" "j15" "l14" "m12" "m13" "p14" "p18" "t10" "z16"
#> 
#> $`4`
#> [1] "d21" "e15" "m19" "o18" "p17" "w16" "x20" "x22"
#> 
#> $`5`
#> [1] "g20" "h24" "k26" "m21" "t22" "v19" "y23" "z25"
#> 
#> $`6`
#>  [1] "b1"  "b2"  "e6"  "e30" "g28" "l29" "n5"  "n27" "p3"  "w4"  "y1"  "z2" 
#> 
#> $`7`
#>  [1] "b8"  "e7"  "f13" "g3"  "g5"  "h9"  "n4"  "r10" "r11" "s9"  "t7"  "t8" 
#> [13] "y12" "z6" 
#> 
#> $`8`
#>  [1] "d11" "g17" "j15" "l14" "m12" "m13" "p14" "p18" "t10" "z16"
#> 
#> $`9`
#> [1] "d21" "e15" "m19" "o18" "p17" "w16" "x20" "x22"
#> 
#> $`10`
#> [1] "g20" "h24" "k26" "m21" "t22" "v19" "y23" "z25"
#> 

# Now make some list vectors into factors
xF <- xL;
xF$c <- factor(xL$c)
# for fun, reverse the levels
xF$c <- factor(xF$c,
   levels=rev(levels(xF$c)))
xF
#> $a
#>  [1] "b1"  "z2"  "p3"  "w4"  "n5"  "e6"  "n27" "g28" "l29" "e30" "y1"  "b2" 
#> 
#> $b
#>  [1] "e7"  "b8"  "h9"  "r10" "r11" "y12" "f13" "g3"  "n4"  "g5"  "z6"  "t7" 
#> [13] "t8"  "s9" 
#> 
#> $c
#>  [1] l14 j15 z16 g17 p18 t10 d11 m12 m13 p14
#> Levels: z16 t10 p18 p14 m13 m12 l14 j15 g17 d11
#> 
#> $d
#> [1] "m19" "x20" "d21" "x22" "e15" "w16" "p17" "o18"
#> 
#> $e
#> [1] "y23" "h24" "z25" "k26" "v19" "g20" "m21" "t22"
#> 
mixedSorts(xF)
#> $a
#>  [1] "b1"  "b2"  "e6"  "e30" "g28" "l29" "n5"  "n27" "p3"  "w4"  "y1"  "z2" 
#> 
#> $b
#>  [1] "b8"  "e7"  "f13" "g3"  "g5"  "h9"  "n4"  "r10" "r11" "s9"  "t7"  "t8" 
#> [13] "y12" "z6" 
#> 
#> $c
#>  [1] z16 t10 p18 p14 m13 m12 l14 j15 g17 d11
#> Levels: z16 t10 p18 p14 m13 m12 l14 j15 g17 d11
#> 
#> $d
#> [1] "d21" "e15" "m19" "o18" "p17" "w16" "x20" "x22"
#> 
#> $e
#> [1] "g20" "h24" "k26" "m21" "t22" "v19" "y23" "z25"
#> 

# test super-long list
xF10k <- rep(xF, length.out=10000);
names(xF10k) <- as.character(seq_along(xF10k));
print(head(mixedSorts(xF10k), 10))
#> $`1`
#>  [1] "b1"  "b2"  "e6"  "e30" "g28" "l29" "n5"  "n27" "p3"  "w4"  "y1"  "z2" 
#> 
#> $`2`
#>  [1] "b8"  "e7"  "f13" "g3"  "g5"  "h9"  "n4"  "r10" "r11" "s9"  "t7"  "t8" 
#> [13] "y12" "z6" 
#> 
#> $`3`
#>  [1] z16 t10 p18 p14 m13 m12 l14 j15 g17 d11
#> Levels: z16 t10 p18 p14 m13 m12 l14 j15 g17 d11
#> 
#> $`4`
#> [1] "d21" "e15" "m19" "o18" "p17" "w16" "x20" "x22"
#> 
#> $`5`
#> [1] "g20" "h24" "k26" "m21" "t22" "v19" "y23" "z25"
#> 
#> $`6`
#>  [1] "b1"  "b2"  "e6"  "e30" "g28" "l29" "n5"  "n27" "p3"  "w4"  "y1"  "z2" 
#> 
#> $`7`
#>  [1] "b8"  "e7"  "f13" "g3"  "g5"  "h9"  "n4"  "r10" "r11" "s9"  "t7"  "t8" 
#> [13] "y12" "z6" 
#> 
#> $`8`
#>  [1] z16 t10 p18 p14 m13 m12 l14 j15 g17 d11
#> Levels: z16 t10 p18 p14 m13 m12 l14 j15 g17 d11
#> 
#> $`9`
#> [1] "d21" "e15" "m19" "o18" "p17" "w16" "x20" "x22"
#> 
#> $`10`
#> [1] "g20" "h24" "k26" "m21" "t22" "v19" "y23" "z25"
#> 

# Make a nested list
set.seed(1);
l1 <- list(
   A=sample(nameVector(11:13, rev(letters[11:13]))),
   B=list(
      C=sample(nameVector(4:8, rev(LETTERS[4:8]))),
      D=sample(nameVector(LETTERS[2:5], rev(LETTERS[2:5])))
   )
)
l1;
#> $A
#>  m  l  k 
#> 11 12 13 
#> 
#> $B
#> $B$C
#> H G D F E 
#> 4 5 8 6 7 
#> 
#> $B$D
#>   D   B   E   C 
#> "C" "E" "B" "D" 
#> 
#> 
# The output is a nested list with the same structure
mixedSorts(l1);
#> $A
#>  m  l  k 
#> 11 12 13 
#> 
#> $B
#> $B$C
#> H G F E D 
#> 4 5 6 7 8 
#> 
#> $B$D
#>   E   D   C   B 
#> "B" "C" "D" "E" 
#> 
#> 
mixedSorts(l1, sortByName=TRUE);
#> $A
#>  k  l  m 
#> 13 12 11 
#> 
#> $B
#> $B$C
#> D E F G H 
#> 8 7 6 5 4 
#> 
#> $B$D
#>   B   C   D   E 
#> "E" "D" "C" "B" 
#> 
#> 

# Make a nested list with two sub-lists
set.seed(1);
l2 <- list(
   A=list(
      E=sample(nameVector(11:13, rev(letters[11:13])))
   ),
   B=list(
      C=sample(nameVector(4:8, rev(LETTERS[4:8]))),
      D=sample(nameVector(LETTERS[2:5], rev(LETTERS[2:5])))
   )
)
l2;
#> $A
#> $A$E
#>  m  l  k 
#> 11 12 13 
#> 
#> 
#> $B
#> $B$C
#> H G D F E 
#> 4 5 8 6 7 
#> 
#> $B$D
#>   D   B   E   C 
#> "C" "E" "B" "D" 
#> 
#> 
# The output is a nested list with the same structure
mixedSorts(l2);
#> $A
#> $A$E
#>  m  l  k 
#> 11 12 13 
#> 
#> 
#> $B
#> $B$C
#> H G F E D 
#> 4 5 6 7 8 
#> 
#> $B$D
#>   E   D   C   B 
#> "B" "C" "D" "E" 
#> 
#> 
mixedSorts(l2, sortByName=TRUE);
#> $A
#> $A$E
#>  k  l  m 
#> 13 12 11 
#> 
#> 
#> $B
#> $B$C
#> D E F G H 
#> 8 7 6 5 4 
#> 
#> $B$D
#>   B   C   D   E 
#> "E" "D" "C" "B" 
#> 
#> 

# when one entry is missing
L0 <- list(A=3:1,
   B=list(C=c(1:3,NA,0),
   D=LETTERS[c(4,5,2)],
   E=NULL));
L0
#> $A
#> [1] 3 2 1
#> 
#> $B
#> $B$C
#> [1]  1  2  3 NA  0
#> 
#> $B$D
#> [1] "D" "E" "B"
#> 
#> $B$E
#> NULL
#> 
#> 
mixedSorts(L0)
#> $A
#> [1] 1 2 3
#> 
#> $B
#> $B$C
#> [1]  0  1  2  3 NA
#> 
#> $B$D
#> [1] "B" "D" "E"
#> 
#> $B$E
#> NULL
#> 
#> 
mixedSorts(L0, na.rm=TRUE)
#> $A
#> [1] 1 2 3
#> 
#> $B
#> $B$C
#> [1]  0  1  2  3 NA
#> 
#> $B$D
#> [1] "B" "D" "E"
#> 
#> $B$E
#> NULL
#> 
#> 
```
