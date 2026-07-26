# Scale a numeric vector from 0 to 1

Scale a numeric vector from 0 to 1

## Usage

``` r
normScale(
  x,
  from = 0,
  to = 1,
  low = min(x, na.rm = TRUE),
  high = max(x, na.rm = TRUE),
  naValue = NA,
  singletMethod = c("mean", "min", "max"),
  ...
)
```

## Arguments

- x:

  `numeric` vector.

- from:

  the minimum `numeric` value to re-scale the input numeric vector.

- to:

  the maximum `numeric` value to re-scale the input numeric vector.

- low:

  `numeric` value defining the low end of the input numeric range,
  intended when input values might not contain the entire numeric range
  to be re-scaled.

- high:

  `numeric` value defining the high end of the input numeric range,
  intended when input values might not contain the entire numeric range
  to be re-scaled.

- naValue:

  optional `numeric` value used to replace `NA`, usually by replacing
  `NA` with zero.

- singletMethod:

  `character` value describing how to handle singlet input values, for
  example how to scale the number 5 by itself.

  - "mean" then it uses the average of `from` and `to`,

  - "min" uses the `from` value, and

  - "max" uses the `to` value.

- ...:

  additional parameters are ignored.

## Value

`numeric` vector after applying the transformations.

## Details

This function is intended as a quick way to scale numeric values between
0 and 1, however other ranges can be defined as needed.

NA values are ignored and will remain NA in the output. To handle NA
values, use the
[`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md) function,
which can optionally replace NA with a fixed numeric value.

The parameters `low` and `high` are used optionally to provide a fixed
range of values expected for `x`, which is useful for consistent scaling
of `x`. Specifically, if `x` may be a vector of numeric values ranging
from 0 and 100, you would define `low=0` and `high=100` so that `x` will
be consistently scaled regardless what actual range is represented by
`x`.

Note that when `x` contains only one value, and `low` and `high` are not
defined, then `x` will be scaled based upon the argument
`singletMethod`. For example, if you provide `x=2` and want to scale `x`
values to between 0 and 10... `x` can either be the `mean` value `5`;
the `min`imum value `0`; or the `max`imum value `10`.

However, if `low` or `high` are defined, then x will be scaled relative
to that range.

## See also

Other jam numeric functions:
[`deg2rad()`](https://jmw86069.github.io/jamba/reference/deg2rad.md),
[`noiseFloor()`](https://jmw86069.github.io/jamba/reference/noiseFloor.md),
[`rad2deg()`](https://jmw86069.github.io/jamba/reference/rad2deg.md),
[`rowGroupMeans()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md),
[`rowRmMadOutliers()`](https://jmw86069.github.io/jamba/reference/rowRmMadOutliers.md),
[`warpAroundZero()`](https://jmw86069.github.io/jamba/reference/warpAroundZero.md)

## Examples

``` r
# Notice the first value 1 is re-scaled to 0
normScale(1:11);
#>  [1] 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0

# Scale values from 0 to 10
normScale(1:11, from=0, to=10);
#>  [1]  0  1  2  3  4  5  6  7  8  9 10

# Here the low value is defined as 0
normScale(1:10, low=0);
#>  [1] 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0

normScale(c(10,20,40,30), from=50, to=65);
#> [1] 50 55 65 60
```
