# Convert radians to degrees

Convert radians to degrees

## Usage

``` r
rad2deg(x, ...)
```

## Arguments

- x:

  `numeric` vector, expected to be radian values between zero and pi\*2.

- ...:

  other parameters are ignored.

## Value

`numeric` vector after coverting radians to degrees.

## Details

This function simply converts radians which range from zero to pi\*2,
into degrees which range from 0 to 360.

## See also

Other jam numeric functions:
[`deg2rad()`](https://jmw86069.github.io/jamba/reference/deg2rad.md),
[`noiseFloor()`](https://jmw86069.github.io/jamba/reference/noiseFloor.md),
[`normScale()`](https://jmw86069.github.io/jamba/reference/normScale.md),
[`rowGroupMeans()`](https://jmw86069.github.io/jamba/reference/rowGroupMeans.md),
[`rowRmMadOutliers()`](https://jmw86069.github.io/jamba/reference/rowRmMadOutliers.md),
[`warpAroundZero()`](https://jmw86069.github.io/jamba/reference/warpAroundZero.md)

## Examples

``` r
rad2deg(c(pi*2, pi/2))
#> [1] 360  90
```
