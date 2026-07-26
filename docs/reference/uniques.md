# apply unique to each element of a list

Apply unique to each element of a list, usually a list of vectors

## Usage

``` r
uniques(
  x,
  keepNames = TRUE,
  incomparables = FALSE,
  useBioc = TRUE,
  useSimpleBioc = FALSE,
  xclass = NULL,
  ...
)
```

## Arguments

- x:

  input list of vectors

- keepNames:

  boolean indicating whether to keep the list element names in the
  returned results.

- incomparables:

  see [`unique()`](https://rdrr.io/r/base/unique.html) for details, this
  value is only sent to `S4Vectors::unique()` when the Bioconductor
  package `S4Vectors` is installed, and is ignored otherwise for
  efficiency.

- useBioc:

  `logical`, default TRUE, indicating whether this function should try
  to use `S4Vectors::unique()` when the Bioconductor package `S4Vectors`
  is installed, otherwise it will use a somewhat less efficient bulk
  operation.

- useSimpleBioc:

  `logical`, default FALSE, whether to use a legacy mechanism with
  `S4Vectors` and is maintained for edge cases where it might be faster.

- xclass:

  `character` optional vector of classes, used to invoke optimized logic
  when the class is known upfront.

- ...:

  additional arguments are ignored.

## Value

`list` with unique values in each list element.

## Details

This function will attempt to use `S4Vectors::unique()` which is
substantially faster than any `apply` family function, especially for
very long lists. However, when `S4Vectors` is not installed, it applies
uniqueness to the `unlist`ed vector of values, which is also
substantially faster than the `apply` family functions for long lists,
but which may still be less efficient than the C implementation provided
by `S4Vectors`.

## See also

Other jam list functions:
[`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md),
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
[`unnestList()`](https://jmw86069.github.io/jamba/reference/unnestList.md)

## Examples

``` r
L1 <- list(CA=nameVector(LETTERS[c(1:4,2,7,4,6)]),
   B=letters[c(7:11,9,3)],
   C2=NULL,
   D=nameVector(LETTERS[4]));
L1;
#> $CA
#>    A B_v1    C D_v1 B_v2    G D_v2    F 
#>  "A"  "B"  "C"  "D"  "B"  "G"  "D"  "F" 
#> 
#> $B
#> [1] "g" "h" "i" "j" "k" "i" "c"
#> 
#> $C2
#> NULL
#> 
#> $D
#>   D 
#> "D" 
#> 
uniques(L1);
#> $CA
#>    A B_v1    C D_v1    G    F 
#>  "A"  "B"  "C"  "D"  "G"  "F" 
#> 
#> $B
#>                         
#> "g" "h" "i" "j" "k" "c" 
#> 
#> $C2
#> NULL
#> 
#> $D
#>   D 
#> "D" 
#> 

uniques(L1, useBioc=FALSE);
#> $CA
#>    A B_v1    C D_v1    G    F 
#>  "A"  "B"  "C"  "D"  "G"  "F" 
#> 
#> $B
#>                         
#> "g" "h" "i" "j" "k" "c" 
#> 
#> $C2
#> character(0)
#> 
#> $D
#>   D 
#> "D" 
#> 
```
