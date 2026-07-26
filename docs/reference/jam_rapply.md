# Jam-specific recursive apply

Jam-specific recursive apply

## Usage

``` r
jam_rapply(x, FUN, how = c("unlist", "list"), ...)
```

## Arguments

- x:

  `list`

- FUN:

  `function` to be called on non-list elements in `x`.

- how:

  `character` string indicating whether to return the `list` or whether
  to call [`unlist()`](https://rdrr.io/r/base/unlist.html) on the
  result.

- ...:

  additional arguments are passed to `FUN`.

## Value

`vector` or `list` based upon argument `how`.

## Details

This function is a very lightweight customization to
[`base::rapply()`](https://rdrr.io/r/base/rapply.html), specifically
that it does not remove 'NULL' entries.

## See also

Other jam list functions:
[`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md),
[`heads()`](https://jmw86069.github.io/jamba/reference/heads.md),
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
L <- list(entryA=c("miR-112", "miR-12", "miR-112"),
   entryB=factor(c("A","B","A","B"),
      levels=c("B","A")),
   entryC=factor(c("C","A","B","B","C"),
      levels=c("A","B","C")),
   entryNULL=NULL)
rapply(L, length)
#> entryA entryB entryC 
#>      3      4      5 
jam_rapply(L, length)
#>    entryA    entryB    entryC entryNULL 
#>         3         4         5         0 

L0 <- list(A=1:3, B=list(C=1:3, D=4:5, E=NULL));
rapply(L0, length)
#>   A B.C B.D 
#>   3   3   2 
jam_rapply(L0, length)
#>   A B.C B.D B.E 
#>   3   3   2   0 
```
