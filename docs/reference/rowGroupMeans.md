# Calculate row group means, or other statistics

Calculate row group means, or other statistics, where: `rowGroupMeans()`
calculates row summary stats; and `rowGroupRmOutliers()` is a
convenience function to call
`rowGroupMeans(..., rmOutliers=TRUE, returnType="input")`.

## Usage

``` r
rowGroupMeans(
  x,
  groups,
  na.rm = TRUE,
  useMedian = TRUE,
  rmOutliers = FALSE,
  crossGroupMad = TRUE,
  madFactor = 5,
  returnType = c("output", "input"),
  rowStatsFunc = NULL,
  groupOrder = c("same", "sort"),
  keepNULLlevels = FALSE,
  includeAttributes = FALSE,
  verbose = FALSE,
  ...
)

rowGroupRmOutliers(
  x,
  groups,
  na.rm = TRUE,
  rmOutliers = TRUE,
  crossGroupMad = TRUE,
  madFactor = 5,
  returnType = c("input"),
  groupOrder = c("same", "sort"),
  keepNULLlevels = FALSE,
  includeAttributes = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  `numeric` data matrix

- groups:

  `character` or `factor` vector of group labels, either as a character
  vector, or a factor. See the parameter `groupOrder` for ordering of
  group labels in the output data matrix.

- na.rm:

  `logical`, default TRUE, passed to the stats func to ignore NA values.

- useMedian:

  `logical`, default TRUE, indicating whether the default stat should be
  "mean" or "median".

- rmOutliers:

  `logical`, default FALSE, indicating whether to apply outlier
  detection and removal.

- crossGroupMad:

  `logical` indicating whether to calculate row MAD values using the
  median across groups for each row. The median is calculated using
  non-NA and non-zero row group MAD values. When `crossGroupMad=TRUE` it
  also calculates the non-NA, non-zero median row MAD across all rows,
  which defines the minimum difference from median applied across all
  values to be considered an outlier.

- madFactor:

  `numeric` value indicating the multiple of the MAD value to define
  outliers. For example `madFactor=5` will take the MAD value for a
  group multiplied by 5, 5*MAD, as a threshold for outliers. So any
  points more than 5*MAD distance from the median per group are
  outliers.

- returnType:

  `character`, default "output", the return data type:

  - `"output"` returns one summary stat value per group, per row;

  - `"input"` is useful when `rmOutliers=TRUE` in that it returns a
    matrix with the same dimensions as the input, except with outlier
    points replaced with NA.

- rowStatsFunc:

  `function`, default NULL, which takes a numeric matrix as input, and
  returns a numeric vector equal to the number of rows of the input data
  matrix. When supplied, `useMedian` is ignored. Examples:
  [`base::rowMeans()`](https://rdrr.io/r/base/colSums.html),
  [`matrixStats::rowMedians()`](https://rdrr.io/pkg/matrixStats/man/rowMedians.html),
  [`matrixStats::rowMads`](https://rdrr.io/pkg/matrixStats/man/rowSds.html).

- groupOrder:

  `character` string indicating how character group labels are ordered
  in the final data matrix, when `returnType="output"`. Note that when
  `groups` is a factor, the factor levels are kept in that order.
  Otherwise, `"same"` keeps groups in the same order they appear in the
  input matrix; `"sort"` applies
  [`jamba::mixedSort()`](https://jmw86069.github.io/jamba/reference/mixedSort.md)
  to the labels.

- keepNULLlevels:

  `logical`, default FALSE, whether to keep factor levels even when
  there are no corresponding columns in `x`. When `TRUE` and
  `returnType="output"` the output matrix will contain one colname for
  each factor level, with NA values used to fill empty factor levels.
  This mechanism can be helpful to ensure that output matrices have
  consistent colnames.

- includeAttributes:

  `logical`, default FALSE, whether to include attributes with `"n"`
  number of replicates per group, and `"nLabel"` with replicate label in
  `n=#` form.

- verbose:

  `logical` indicating whether to print verbose output.

- ...:

  additional parameters are passed to `rowStatsFunc`, and if
  `rmOutliers=TRUE` to
  [`jamba::rowRmMadOutliers()`](https://jmw86069.github.io/jamba/reference/rowRmMadOutliers.md).

## Value

`numeric` matrix based upon `returnType`:

- When `returnType="output"` the output is a numeric matrix with the
  same number of columns as the number of unique `groups` labels. When
  `groups` is a factor and `keepNULLlevels=TRUE`, the number of columns
  will be the number of factor levels, otherwise it will be the number
  of factor levels used in `groups`.

- When `returnType="input"` the output is a numeric matrix with the same
  dimensions as the input data. This output is intended for use with
  `rmOutliers=TRUE` which will replace outlier points with `NA` values.
  Therefore, this matrix can be used to see the location of outliers.

The function also returns attributes when `includeAttributes=TRUE`,
although the default is FALSE. The attributes describe the number of
samples per group overall:

- attr(out, "n"):

  The attribute `"n"` is used to describe the number of replicates per
  group.

- attr(out, "nLabel"):

  The attribute `"nLabel"` is a simple text label in the form `"n=3"`.

Note that when `rmOutliers=TRUE` the number of replicates per group will
vary depending upon the outliers removed. In that case, remember that
the reported `"n"` is always the total possible columns available prior
to outlier removal.

## Details

This function by default calculates group mean values per row in a
numeric matrix. However, the stat function can be changed to calculate
row medians, row MADs, etc.

An added purpose of this function is optional outlier filtering, via
calculation of MAD values and applying a MAD threshold cutoff. The
intention is to identify technical outliers that otherwise adversely
affect the calculated group mean or median values. To inspect the data
after outlier removal, use the parameter `returnType="input"` which will
return the input data matrix with `NA` substituted for outlier points.
Outlier detection and removal is performed by
[`jamba::rowRmMadOutliers()`](https://jmw86069.github.io/jamba/reference/rowRmMadOutliers.md).

## See also

Other jam numeric functions:
[`deg2rad()`](https://jmw86069.github.io/jamba/reference/deg2rad.md),
[`noiseFloor()`](https://jmw86069.github.io/jamba/reference/noiseFloor.md),
[`normScale()`](https://jmw86069.github.io/jamba/reference/normScale.md),
[`rad2deg()`](https://jmw86069.github.io/jamba/reference/rad2deg.md),
[`rowRmMadOutliers()`](https://jmw86069.github.io/jamba/reference/rowRmMadOutliers.md),
[`warpAroundZero()`](https://jmw86069.github.io/jamba/reference/warpAroundZero.md)

## Examples

``` r
x <- matrix(ncol=9, stats::rnorm(90));
colnames(x) <- LETTERS[1:9];
use_groups <- rep(letters[1:3], each=3)
rowGroupMeans(x, groups=use_groups)
#>                a            b           c
#>  [1,]  0.3297912 -0.006198262 -0.66518864
#>  [2,] -1.1655448  0.634362125  0.45203019
#>  [3,] -0.8185157 -0.279333528  0.30027912
#>  [4,]  0.2865486  0.793585308  0.07485682
#>  [5,] -0.3200564 -0.241689768  0.20637270
#>  [6,] -0.4321298 -0.374800093  1.76365303
#>  [7,]  0.8001769 -0.772978228  0.03768285
#>  [8,] -0.1294107  0.084543768 -0.04691673
#>  [9,]  0.8867361 -1.334353628  0.15161137
#> [10,] -0.9343851  0.495870480  1.29230591

# rowGroupRmOutliers returns the input data after outlier removal
rowGroupRmOutliers(x, groups=use_groups, returnType="input")
#>                A          B           C          D          E            F
#>  [1,] -0.1453936  0.3297912  0.39370865 -0.5208693  1.2339762 -0.006198262
#>  [2,] -1.1655448 -3.2273228  0.40363146  1.6232025  0.6343621 -0.685706846
#>  [3,] -0.8185157 -0.7717918 -0.88643672 -1.0700682  0.4120223 -0.279333528
#>  [4,]  0.6849361  0.2865486 -1.31893760  1.6858872  0.7935853 -0.782730275
#>  [5,] -0.3200564         NA  0.02884391 -0.2416898 -0.1524106 -0.778997240
#>  [6,]         NA         NA -0.43212979 -0.4682005 -0.2288958 -0.374800093
#>  [7,] -0.5996083  0.8001769  1.68987252 -0.7729782 -0.9007918 -0.319393809
#>  [8,] -0.1294107 -0.1639310  1.22839278         NA -0.7350262  0.084543768
#>  [9,]  0.8867361  1.2429188  0.27602348 -1.3343536 -1.4276858 -0.768473603
#> [10,] -0.1513960 -0.9343851 -1.04897550  0.4958705  0.6192835           NA
#>                 G           H          I
#>  [1,] -0.90087086 -0.46355650 -0.6651886
#>  [2,]  0.66372867  0.30546323  0.4520302
#>  [3,]  0.30027912 -0.08398871  0.5268557
#>  [4,]  0.07485682  0.41036345 -0.2302622
#>  [5,]  0.20637270  0.18367824         NA
#>  [6,]          NA  1.77874162  1.7636530
#>  [7,] -0.62795166  0.03768285  0.4856014
#>  [8,] -0.04691673  1.17622012 -0.2657389
#>  [9,]  0.16261812          NA  0.1516114
#> [10,]  1.29230591          NA  1.3766098

# rowGroupMeans(..., returnType="input") also returns the input data
rowGroupMeans(x, groups=use_groups, rmOutliers=TRUE, returnType="input")
#>                A          B           C          D          E            F
#>  [1,] -0.1453936  0.3297912  0.39370865 -0.5208693  1.2339762 -0.006198262
#>  [2,] -1.1655448 -3.2273228  0.40363146  1.6232025  0.6343621 -0.685706846
#>  [3,] -0.8185157 -0.7717918 -0.88643672 -1.0700682  0.4120223 -0.279333528
#>  [4,]  0.6849361  0.2865486 -1.31893760  1.6858872  0.7935853 -0.782730275
#>  [5,] -0.3200564         NA  0.02884391 -0.2416898 -0.1524106 -0.778997240
#>  [6,]         NA         NA -0.43212979 -0.4682005 -0.2288958 -0.374800093
#>  [7,] -0.5996083  0.8001769  1.68987252 -0.7729782 -0.9007918 -0.319393809
#>  [8,] -0.1294107 -0.1639310  1.22839278         NA -0.7350262  0.084543768
#>  [9,]  0.8867361  1.2429188  0.27602348 -1.3343536 -1.4276858 -0.768473603
#> [10,] -0.1513960 -0.9343851 -1.04897550  0.4958705  0.6192835           NA
#>                 G           H          I
#>  [1,] -0.90087086 -0.46355650 -0.6651886
#>  [2,]  0.66372867  0.30546323  0.4520302
#>  [3,]  0.30027912 -0.08398871  0.5268557
#>  [4,]  0.07485682  0.41036345 -0.2302622
#>  [5,]  0.20637270  0.18367824         NA
#>  [6,]          NA  1.77874162  1.7636530
#>  [7,] -0.62795166  0.03768285  0.4856014
#>  [8,] -0.04691673  1.17622012 -0.2657389
#>  [9,]  0.16261812          NA  0.1516114
#> [10,]  1.29230591          NA  1.3766098

# rowGroupMeans with outlier removal
rowGroupMeans(x, groups=use_groups, rmOutliers=TRUE)
#>                a            b           c
#>  [1,]  0.3297912 -0.006198262 -0.66518864
#>  [2,] -1.1655448  0.634362125  0.45203019
#>  [3,] -0.8185157 -0.279333528  0.30027912
#>  [4,]  0.2865486  0.793585308  0.07485682
#>  [5,] -0.1456063 -0.241689768  0.19502547
#>  [6,] -0.4321298 -0.374800093  1.77119732
#>  [7,]  0.8001769 -0.772978228  0.03768285
#>  [8,] -0.1294107 -0.325241194 -0.04691673
#>  [9,]  0.8867361 -1.334353628  0.15711474
#> [10,] -0.9343851  0.557577007  1.33445786
```
