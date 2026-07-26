# return the classes of a list of objects

return the classes of a list of objects

## Usage

``` r
sclass(x, ...)
```

## Arguments

- x:

  an S3 object inheriting from class `list`, or an S4 object.

- ...:

  additional parameters are ignored.

## Value

`character` vector with the class of each list element, or column name,
depending upon the input `class(x)`.

## Details

This function takes a `list` and returns the classes for each object in
the list. In the event an object class has multiple values, the returned
object is a list, otherwise is a vector. If `x` is an S4 object, then
`methods::slotNames(x)` is used, and the class is returned for each S4
slot.

When `x` is a `data.frame`, `data.table`, `tibble`, or similar
`DataFrame` table-like object, the class of each column is returned.

For the special case where `x` is an S4 object with one slotName
`".Data"`, the values in `x@.Data` are coerced to a `list`. One example
of this case is with `limma::MArrayLM-class`.

When `x` is a matrix, the class of each column is returned for
consistency, even though the class of each column should be identical.

For more more information about a list-like object, including the
lengths/dimensions of the elements, see
[`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md) or
[`ssdim()`](https://jmw86069.github.io/jamba/reference/sdim.md).

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
[`sdim()`](https://jmw86069.github.io/jamba/reference/sdim.md),
[`uniques()`](https://jmw86069.github.io/jamba/reference/uniques.md),
[`unnestList()`](https://jmw86069.github.io/jamba/reference/unnestList.md)

## Examples

``` r
sclass(list(LETTERS=LETTERS, letters=letters));
#>     LETTERS     letters 
#> "character" "character" 

sclass(data.frame(B=letters[1:10], C=2:11))
#>           B           C 
#> "character"   "integer" 
```
