# Apply head() across each element in a list of vectors

Apply head() across each element in a list of vectors

## Usage

``` r
heads(x, n = 6, ...)
```

## Arguments

- x:

  `list` of atomic vectors, assumed to be the same atomic type.

- n:

  `integer` maximum number of items to include from each element in the
  list `x`. When `n` contains multiple values, they are recycled to
  `length(x)` and applied to each list element in order.

- ...:

  additional arguments are passed to
  [`utils::head()`](https://rdrr.io/r/utils/head.html).

## Value

`list` with at most `n` elements per vector.

## Details

Note that this function currently only operates on a list of vectors.
This function is notably faster than `lapply(x, head, n)` because it
operates on the entire vector in one step.

Also the input `n` can be a vector so that each element in the list has
a specific number of items returned.

## See also

Other jam list functions:
[`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md),
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
l <- list(a=1:10, b=2:5, c=NULL, d=1:100);
heads(l, 1);
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $c
#> integer(0)
#> 
#> $d
#> [1] 1
#> 

heads(l, 2);
#> $a
#> [1] 1 2
#> 
#> $b
#> [1] 2 3
#> 
#> $c
#> integer(0)
#> 
#> $d
#> [1] 1 2
#> 

heads(l, n=c(2, 1, 3, 5))
#> $a
#> [1] 1 2
#> 
#> $b
#> [1] 2
#> 
#> $c
#> integer(0)
#> 
#> $d
#> [1] 1 2 3 4 5
#> 
```
