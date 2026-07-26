# Remove outlier points per row by MAD factor threshold

Remove outlier points per row by MAD factor threshold

## Usage

``` r
rowRmMadOutliers(
  x,
  madFactor = 5,
  na.rm = TRUE,
  minDiff = 0,
  minReps = 3,
  includeAttributes = FALSE,
  rowMadValues = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  numeric matrix

- madFactor:

  `numeric` value to multiply by each row MAD to define the threshold
  for outlier detection.

- na.rm:

  `logical` indicating whether to ignore NA values when calculating the
  MAD value. It should probably always be `TRUE`, however setting to
  `FALSE` will prevent any calculations in rows that contain `NA`
  values, which could be useful.

- minDiff:

  `numeric` value indicating the minimum difference from median to
  qualify as an outlier. This value protects against removing outliers
  which are already extremely similar. Consider this example:

  - Three numeric values: `c(10.0001, 10.0002, 10.001)`.

  - The third value differs from median by only 0.0008.

  - The third value `10.001` is 5x MAD factor away from median.

  - `minDiff = 0.01` would require the minimum difference from median to
    be at least 0.01 to be eligible to be an outlier point.

- minReps:

  `numeric` minimum number of non-NA values per row for outliers to be
  filtered on the row. This argument is typically only relevant for rows
  with `n=2` non-NA values, and when `rowMadValues` is supplied and may
  define a threshold less than half the difference in the two points on
  the given row. Otherwise, n=2 defines each point at exactly 1x MAD
  from median, and would therefore never be considered an outlier.

- includeAttributes:

  `logical` indicating whether to return attributes that describe the
  threshold and type of threshold used per row, in addition to the
  madFactor and minDiff values defined.

- rowMadValues:

  `numeric` optional set of row MAD values to use, which is mostly
  helpful when combining MAD values across multiple samples groups on
  each row of data, where the combined MAD values may be more reliable
  than individual group MAD values.

- verbose:

  `logical` indicating whether to print verbose output.

- ...:

  additional parameters are ignored.

## Value

`numeric` matrix with the same dimensions as the input `x` matrix.
Outliers are replaced with `NA`.

If `includeAttributes=TRUE` then attributes will be included:

- `outlierDF` which is a `data.frame` with colnames

  - rowMedians: `numeric` median on each row

  - rowMadValues: `numeric` MAD for each row

  - rowThresholds: `numeric` threshold after applying `madFactor` and
    `minDiff`

  - rowReps: `integer` number of non-NA values in the input data

  - rowTypes: `factor` indicating the type of threshold: `"madFactor"`
    means the row applied the normal `MAD * madFactor` threshold;
    `"minDiff"` means the row applied the `minDiff` threshold which was
    the larger threshold.

- `minDiff` with the `numeric` value supplied

- `madFactor` with the `numeric` MAD factor threshold supplied

- `outliersRemoved` with the `integer` total number of new NA values
  produced by the outlier removal process.

## Details

This function applies outlier detection and removal per row of the input
numeric matrix.

- It first calculates MAD per row.

- The MAD threshold cutoff is a multiple of the MAD value, defined by
  `madFactor`, multiplying the per-row MAD by the `madFactor`.

- The absolute difference from median is calculated for each point.

- Outlier points are defined:

  1.  Points with MAD above the MAD threshold, and

  2.  Points with difference from median at or above `minDiff`

The `minDiff` parameter affects cases such as 3 replicates, where all
replicates are well within a known threshold indicating low variance,
but where two replicates might be nearly identical. Consider:

- Three numeric values: `c(10.0001, 10.0002, 10.001)`.

- The third value differs from median by only 0.0008.

- The third value `10.001` is 5x MAD factor away from median.

- `minDiff = 0.01` would require the minimum difference from median to
  be at least 0.01 to be eligible to be an outlier point.

One option to define `minDiff` from the data is to use:
`minDiff <- stats::median(rowMads(x))`

In this case, the threshold is defined by the median difference from
median across all rows. This type of threshold will only be reasonable
if the variance across all rows is expected to be fairly similar.

This function is substantially faster when the `matrixStats` package is
installed, but will use the `apply(x, 1, mad)` format as a last option.

### Assumptions

1.  This function assumes the input data is appropriate for the use of
    MAD as a summary statistic.

2.  Specifically, numeric values per row are expected to be roughly
    normally distributed.

3.  Outlier points are assumed to be present in less than half overall
    non-NA data.

4.  Outlier points are assumed to be technical outliers, and therefore
    not the direct result of the experimental measurements being
    studied. Technical outliers are often caused by some instrument
    measurement, methodological failure, or other upstream protocol
    failure.

The default threshold of 5x MAD factor is a fairly lenient criteria,
above which the data may even be assumed not to conform to most
downstream statistical techniques.

For measurements considered to be more robust, or required to be more
robust, the threshold 2x MAD is applied. This criteria is usually a
reasonable expectation of housekeeper gene expression across replicates
within each sample group.

## See also

Other jam numeric functions:
[`deg2rad()`](https://jmw86069.github.io/jamba/reference/deg2rad.md),
[`noiseFloor()`](https://jmw86069.github.io/jamba/reference/noiseFloor.md),
[`normScale()`](https://jmw86069.github.io/jamba/reference/normScale.md),
[`rad2deg()`](https://jmw86069.github.io/jamba/reference/rad2deg.md),
[`rowGroupMeans()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md),
[`warpAroundZero()`](https://jmw86069.github.io/jamba/reference/warpAroundZero.md)

## Examples

``` r
set.seed(123);
x <- matrix(ncol=5, stats::rnorm(25))*5 + 10;
## Define some outlier points
x[1:2,3] <- x[1:2,3]*5 + 50;
x[2:3,2] <- x[2:3,2]*5 - 100;
rownames(x) <- head(letters, nrow(x));

rowRmMadOutliers(x, madFactor=5);
#>        [,1]      [,2]      [,3]       [,4]     [,5]
#> a  7.197622 18.575325        NA 18.9345657 4.660881
#> b  8.849113        NA        NA 12.4892524 8.910125
#> c 17.793542        NA 12.003857  0.1669142 4.869978
#> d 10.352542  6.565736 10.553414 13.5067795 6.355544
#> e 10.646439  7.771690  7.220794  7.6360430 6.874804

x2 <- rowRmMadOutliers(x, madFactor=2,
   includeAttributes=TRUE);
x2
#>        [,1]      [,2]      [,3]       [,4]     [,5]
#> a  7.197622 18.575325        NA 18.9345657 4.660881
#> b  8.849113        NA        NA 12.4892524 8.910125
#> c 17.793542        NA 12.003857  0.1669142 4.869978
#> d 10.352542  6.565736 10.553414 13.5067795 6.355544
#> e        NA  7.771690  7.220794  7.6360430 6.874804
#> attr(,"outlierDF")
#>   rowMedians rowMadValues rowThresholds rowReps  rowTypes
#> a  18.575325   16.8685827     33.737165       5 madFactor
#> b   8.910125    5.3064136     10.612827       5 madFactor
#> c   4.869978   10.5766897     21.153379       5 madFactor
#> d  10.352542    4.6764726      9.352945       5 madFactor
#> e   7.636043    0.6156476      1.231295       5 madFactor
#> attr(,"minDiff")
#> [1] 0
#> attr(,"madFactor")
#> [1] 2
#> attr(,"outliersRemoved")
#> [1] 5

x3 <- rowRmMadOutliers(x2,
   madFactor=2,
   rowMadValues=attr(x2, "outlierDF")$rowMadValues,
   includeAttributes=TRUE);
x3
#>        [,1]      [,2]      [,3]       [,4]     [,5]
#> a  7.197622 18.575325        NA 18.9345657 4.660881
#> b  8.849113        NA        NA 12.4892524 8.910125
#> c 17.793542        NA 12.003857  0.1669142 4.869978
#> d 10.352542  6.565736 10.553414 13.5067795 6.355544
#> e        NA  7.771690  7.220794  7.6360430 6.874804
#> attr(,"outlierDF")
#>   rowMedians rowMadValues rowThresholds rowReps  rowTypes
#> a  12.886473   16.8685827     33.737165       4 madFactor
#> b   8.910125    5.3064136     10.612827       3 madFactor
#> c   8.436918   10.5766897     21.153379       4 madFactor
#> d  10.352542    4.6764726      9.352945       5 madFactor
#> e   7.428419    0.6156476      1.231295       4 madFactor
#> attr(,"minDiff")
#> [1] 0
#> attr(,"madFactor")
#> [1] 2
#> attr(,"outliersRemoved")
#> [1] 0
```
