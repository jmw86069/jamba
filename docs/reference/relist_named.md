# relist a vector which allows re-ordered names

relist a vector which imposes the model object list structure while
allowing vector elements and names to be re-ordered

## Usage

``` r
relist_named(x, skeleton, ...)
```

## Arguments

- x:

  vector to be applied to the `skeleton` list structure in order.

- skeleton:

  `list` object representing the desired final list structure, or
  `vector` when the input data `x` should be returned as-is, without
  change. Specifically, when `skeleton` is a `vector`, the `names(x)`
  are maintained without change.

- ...:

  additional arguments are ignored.

## Value

`list` object with the same structure as the `skeleton`.

## Details

This function is a simple update to
[`utils::relist()`](https://rdrr.io/r/utils/relist.html) that allows the
order of vectors to change, alongside the correct names for each
element.

More specifically, this function does not replace the updated names with
the corresponding names from the list `skeleton`, as is the case in
default implementation of
[`utils::relist()`](https://rdrr.io/r/utils/relist.html).

This function is called by
[`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md)
which iteratively calls
[`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md)
on each vector component of the input `list`, and permits nested lists.
The result is a single sorted vector which is split into the `list`
components, then relist-ed to the original structure. During the
process, it is important to retain vector names in the order defined by
[`mixedOrder()`](https://jmw86069.github.io/jamba/reference/mixedOrder.md).

## See also

Other jam list functions:
[`cPaste()`](https://jmw86069.github.io/jamba/reference/cPaste.md),
[`heads()`](https://jmw86069.github.io/jamba/reference/heads.md),
[`jam_rapply()`](https://jmw86069.github.io/jamba/reference/jam_rapply.md),
[`list2df()`](https://jmw86069.github.io/jamba/reference/list2df.md),
[`mergeAllXY()`](https://jmw86069.github.io/jamba/reference/mergeAllXY.md),
[`mixedSorts()`](https://jmw86069.github.io/jamba/reference/mixedSorts.md),
[`rbindList()`](https://jmw86069.github.io/jamba/reference/rbindList.md),
[`rlengths()`](https://jmw86069.github.io/jamba/reference/rlengths.md),
[`sclass()`](https://jmw86069.github.io/jamba/reference/sclass.md),
[`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md),
[`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md),
[`unnestList()`](https://jmw86069.github.io/jamba/reference/unnestList.md)

## Examples

``` r
# generate nested list
x <- list(A=nameVector(LETTERS[3:1]),
   B=list(
      E=nameVector(LETTERS[10:7]),
      D=nameVector(LETTERS[5:4])),
   C=list(
      G=nameVector(LETTERS[19:16]),
      F=nameVector(LETTERS[15:11]),
      H=list(
         I=nameVector(LETTERS[22:20]))
      ))
x
#> $A
#>   C   B   A 
#> "C" "B" "A" 
#> 
#> $B
#> $B$E
#>   J   I   H   G 
#> "J" "I" "H" "G" 
#> 
#> $B$D
#>   E   D 
#> "E" "D" 
#> 
#> 
#> $C
#> $C$G
#>   S   R   Q   P 
#> "S" "R" "Q" "P" 
#> 
#> $C$F
#>   O   N   M   L   K 
#> "O" "N" "M" "L" "K" 
#> 
#> $C$H
#> $C$H$I
#>   V   U   T 
#> "V" "U" "T" 
#> 
#> 
#> 

# unlisted vector of items
xu <- unlist(unname(x))
# unlisted vector of names
xun <- unname(jam_rapply(x, names));
names(xu) <- xun;

# recursive list element lengths
xrn <- jam_rapply(x, length);
# define factor in order of list structure
xn <- factor(
   rep(names(xrn),
      xrn),
   levels=names(xrn));

# re-create the original list
xu_new <- unlist(unname(split(xu, xn)))
xnew <- relist_named(xu_new, x);
xnew
#> $A
#>   C   B   A 
#> "C" "B" "A" 
#> 
#> $B
#> $B$E
#>   J   I   H   G 
#> "J" "I" "H" "G" 
#> 
#> $B$D
#>   E   D 
#> "E" "D" 
#> 
#> 
#> $C
#> $C$G
#>   S   R   Q   P 
#> "S" "R" "Q" "P" 
#> 
#> $C$F
#>   O   N   M   L   K 
#> "O" "N" "M" "L" "K" 
#> 
#> $C$H
#> $C$H$I
#>   V   U   T 
#> "V" "U" "T" 
#> 
#> 
#> 

# re-order elements
k <- mixedOrder(xu_new);
xuk <- unlist(unname(split(xu[k], xn[k])))
xk <- relist_named(xuk, x);
xk
#> $A
#>   A   B   C 
#> "A" "B" "C" 
#> 
#> $B
#> $B$E
#>   G   H   I   J 
#> "G" "H" "I" "J" 
#> 
#> $B$D
#>   D   E 
#> "D" "E" 
#> 
#> 
#> $C
#> $C$G
#>   P   Q   R   S 
#> "P" "Q" "R" "S" 
#> 
#> $C$F
#>   K   L   M   N   O 
#> "K" "L" "M" "N" "O" 
#> 
#> $C$H
#> $C$H$I
#>   T   U   V 
#> "T" "U" "V" 
#> 
#> 
#> 

# the default relist() function does not support this use case
xdefault <- relist(xuk, x);
xdefault
#> $A
#>   C   B   A 
#> "A" "B" "C" 
#> 
#> $B
#> $B$E
#>   J   I   H   G 
#> "G" "H" "I" "J" 
#> 
#> $B$D
#>   E   D 
#> "D" "E" 
#> 
#> 
#> $C
#> $C$G
#>   S   R   Q   P 
#> "P" "Q" "R" "S" 
#> 
#> $C$F
#>   O   N   M   L   K 
#> "K" "L" "M" "N" "O" 
#> 
#> $C$H
#> $C$H$I
#>   V   U   T 
#> "T" "U" "V" 
#> 
#> 
#> 
```
