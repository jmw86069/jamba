# Convert degrees to radians

Convert degrees to radians

## Usage

``` r
deg2rad(x, ...)
```

## Arguments

- x:

  `numeric` vector, expected to be degree values between zero and 360.

- ...:

  other parameters are ignored.

## Value

`numeric` vector after coverting degrees to radians.

## Details

This function simply converts degrees which range from 0 to 360, into
radians which range from zero to pi\*2.

## See also

Other jam numeric functions:
[`noiseFloor()`](https://jmw86069.github.io/jamba/reference/noiseFloor.md),
[`normScale()`](https://jmw86069.github.io/jamba/reference/normScale.md),
[`rad2deg()`](https://jmw86069.github.io/jamba/reference/rad2deg.md),
[`rowGroupMeans()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md),
[`rowRmMadOutliers()`](https://jmw86069.github.io/jamba/reference/rowRmMadOutliers.md),
[`warpAroundZero()`](https://jmw86069.github.io/jamba/reference/warpAroundZero.md)

## Examples

``` r
deg2rad(rad2deg(c(pi*2, pi/2)))/pi;
#> [1] 2.0 0.5
```
