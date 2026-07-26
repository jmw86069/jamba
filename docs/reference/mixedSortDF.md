# sort data.frame keeping numeric values in proper order

sort data.frame keeping numeric values in proper order

## Usage

``` r
mixedSortDF(
  df,
  byCols = seq_len(ncol(df)),
  na.last = TRUE,
  decreasing = NULL,
  useRownames = FALSE,
  verbose = FALSE,
  blanksFirst = TRUE,
  keepNegative = FALSE,
  keepInfinite = FALSE,
  keepDecimal = FALSE,
  ignore.case = TRUE,
  useCaseTiebreak = TRUE,
  sortByName = FALSE,
  honorFactor = TRUE,
  ...
)
```

## Arguments

- df:

  `data.frame` input

- byCols:

  one of two types of input:

  1.  `integer` vector referring to the order of columns to be used by
      [`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md)
      to order the `data.frame`. Note that negative values will reverse
      the sort order for the corresponding column number. To sort
      `rownames(df)` use zero `0`, and to reverse sorting `rownames(x)`
      use `-0.1` where the negative sign will reverse the sort, and
      `-0.1` will be rounded to `0`.

  2.  `character` vector of values in `colnames(df)`, optionally
      including prefix `"-"` to reverse the sort. Note that the argument
      `decreasing` can also be used to specify columns to have reverse
      sort, either as a single value or vector to be applied to each
      column in `byCols`. To sort `rownames(df)` use `"rownames"` or
      `"row.names"`. To reverse sorting `rownames(df)` use `"-rownames"`
      or `"-row.names"`.

- na.last:

  `logical` whether to move NA entries to the end of the sort. When
  `na.last=TRUE` then `NA` values will always be last, even following
  blanks and infinite values. When `na.last=FALSE` then `NA` values will
  always be first, even before blanks and negative infinite values.

- decreasing:

  NULL or `logical` vector indicating which columns in `byCols` should
  be sorted in decreasing order. By default, the `sign(byCols)` is used
  to define the sort order of each column, but it can be explicitly
  overridden with this `decreasing` parameter.

- useRownames:

  `logical` whether to use `rownames(df)` as a last tiebreaker in the
  overall rank ordering. This parameter has the primary effect of
  assuring a reproducible result, provided the rownames are consistently
  defined, or if rownames are actually row numbers. When
  `useRownames=FALSE` then rows that would otherwise be ties will be
  returned in the same order they were provided in `df`.

- verbose:

  `logical` whether to print verbose output. When `verbose=2` there is
  slightly more verbose output.

- blanksFirst, keepNegative, keepInfinite, keepDecimal, ignore.case,
  useCaseTiebreak, sortByName:

  arguments passed to
  [`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md),
  except `sortByName` which is not passed along.

- honorFactor:

  `logical`, default TRUE, indicating whether to honor factor level
  order in the output, otherwise when FALSE it sorts as `character`.

- ...:

  additional arguments passed to
  [`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md)
  for custom sort options as described in
  [`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md).

## Value

`data.frame` whose rows are ordered using
[`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md).

## Details

This function is a wrapper around
[`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md)
so it operates on `data.frame` columns in the proper order, using logic
similar that used by
[`base::order()`](https://rdrr.io/r/base/order.html) when operating on a
`data.frame`. The sort order logic is fully described in
[`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
and
[`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md).

Note that `byCols` can either be given as `integer` column index values,
or `character` vector of `colnames(x)`. In either case, using negative
prefix `-` will reverse the sort order of the corresponding column.

For example `byCols=c(2, -1)` will sort column 2 increasing, then column
1 decreasing.

Similarly, one can supply `colnames(df)`, such as
`byCols=c("colname2", "-colname1")`. Values are matched as-is to
`colnames(df)` first, then any values not matched are compared again
after removing prefix `-` from the start of each `character` string.
Therefore, if `colnames(df)` contains `"-colname1"` it will be matched
as-is, but `"--colname1"` will only be matched after removing the first
`-`, after which the sort order will be reversed for that column.

For direct control over the sort order of each column defined in
`byCols`, you can supply `logical` vector to argument `decreasing`, and
this vector is recycled to `length(byCols)`.

Finally, for slight efficiency, only unique columns defined in `byCols`
are used to determine the row order, so even if a column is defined
twice in `byCols`, only the first instance is passed to
[`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md)
to determine row order.

## See also

Other jam sort functions:
[`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md),
[`mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md),
[`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md),
[`mmixedOrder()`](https://jmw86069.github.io/jamba/reference/mmixedOrder.md)

## Examples

``` r
# start with a vector of miRNA names
x <- c("miR-12","miR-1","miR-122","miR-1b", "miR-1a","miR-2");
# add some arbitrary group information
g <- rep(c("Air", "Treatment", "Control"), 2);
# create a data.frame
df <- data.frame(group=g,
   miRNA=x,
   stringsAsFactors=FALSE);

# input data
df;
#>       group   miRNA
#> 1       Air  miR-12
#> 2 Treatment   miR-1
#> 3   Control miR-122
#> 4       Air  miR-1b
#> 5 Treatment  miR-1a
#> 6   Control   miR-2

# output when using order()
df[do.call(order, df), , drop=FALSE];
#>       group   miRNA
#> 1       Air  miR-12
#> 4       Air  miR-1b
#> 3   Control miR-122
#> 6   Control   miR-2
#> 2 Treatment   miR-1
#> 5 Treatment  miR-1a

# output with mixedSortDF()
mixedSortDF(df);
#>       group   miRNA
#> 4       Air  miR-1b
#> 1       Air  miR-12
#> 6   Control   miR-2
#> 3   Control miR-122
#> 2 Treatment   miR-1
#> 5 Treatment  miR-1a

# mixedSort respects factor order
# reorder factor levels to demonstrate.
# "Control" should come first
gf <- factor(g, levels=c("Control", "Air", "Treatment"));
df2 <- data.frame(groupfactor=gf,
   miRNA=x,
   stringsAsFactors=FALSE);

# now the sort properly keeps the group factor levels in order,
# which also sorting the miRNA names in their proper order.
mixedSortDF(df2);
#>   groupfactor   miRNA
#> 6     Control   miR-2
#> 3     Control miR-122
#> 4         Air  miR-1b
#> 1         Air  miR-12
#> 2   Treatment   miR-1
#> 5   Treatment  miR-1a


x <- data.frame(l1=letters[1:10],
   l2=rep(letters[1:2+10], 5),
   L1=LETTERS[1:10],
   L2=rep(LETTERS[1:2+20], each=5));
set.seed(123);
rownames(x) <- sample(seq_len(10));
x;
#>    l1 l2 L1 L2
#> 3   a  k  A  U
#> 10  b  l  B  U
#> 2   c  k  C  U
#> 8   d  l  D  U
#> 6   e  k  E  U
#> 9   f  l  F  V
#> 1   g  k  G  V
#> 7   h  l  H  V
#> 5   i  k  I  V
#> 4   j  l  J  V

# sort by including rownames
mixedSortDF(x, byCols=c("rownames"));
#>    l1 l2 L1 L2
#> 1   g  k  G  V
#> 2   c  k  C  U
#> 3   a  k  A  U
#> 4   j  l  J  V
#> 5   i  k  I  V
#> 6   e  k  E  U
#> 7   h  l  H  V
#> 8   d  l  D  U
#> 9   f  l  F  V
#> 10  b  l  B  U
mixedSortDF(x, byCols=c("L2", "-rownames"));
#>    l1 l2 L1 L2
#> 10  b  l  B  U
#> 8   d  l  D  U
#> 6   e  k  E  U
#> 3   a  k  A  U
#> 2   c  k  C  U
#> 9   f  l  F  V
#> 7   h  l  H  V
#> 5   i  k  I  V
#> 4   j  l  J  V
#> 1   g  k  G  V

# demonstrate sorting a matrix with no rownames
m <- matrix(c(2, 1, 3, 4), ncol=2);
mixedSortDF(m, byCols=-2)
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    3

# add rownames
rownames(m) <- c("c", "a");
mixedSortDF(m, byCols=0)
#>   [,1] [,2]
#> a    1    4
#> c    2    3
mixedSortDF(m, byCols="-rownames")
#>   [,1] [,2]
#> c    2    3
#> a    1    4
mixedSortDF(m, byCols="rownames")
#>   [,1] [,2]
#> a    1    4
#> c    2    3

mixedSortDF(data.frame(factor1=factor(c("Cnot9", "Cnot8", "Cnot10"))), honorFactor=FALSE)
#>   factor1
#> 2   Cnot8
#> 1   Cnot9
#> 3  Cnot10

# test date columns
testfiles <- system.file(package="jamba", c("TODO.md", "README.md", "NEWS.md"))
testinfo <- file.info(testfiles)
testinfo
#>                                                                                            size
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md 119967
#>                                                                                          isdir
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md FALSE
#>                                                                                          mode
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md  666
#>                                                                                                        mtime
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md 2026-07-25 15:58:30
#>                                                                                                        ctime
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md 2026-07-25 15:58:30
#>                                                                                                        atime
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md 2026-07-25 15:58:30
#>                                                                                          exe
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md  no
#>                                                                                               uname
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md James.Ward
#>                                                                                          udomain
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md  SCIOME
mixedSortDF(testinfo, byCols="mtime")
#>                                                                                            size
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md 119967
#>                                                                                          isdir
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md FALSE
#>                                                                                          mode
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md  666
#>                                                                                                        mtime
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md 2026-07-25 15:58:30
#>                                                                                                        ctime
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md 2026-07-25 15:58:30
#>                                                                                                        atime
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md 2026-07-25 15:58:30
#>                                                                                          exe
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md  no
#>                                                                                               uname
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md James.Ward
#>                                                                                          udomain
#> C:/Users/james.ward/AppData/Local/Temp/RtmpOqToiO/temp_libpath7acc523f2764/jamba/NEWS.md  SCIOME
```
