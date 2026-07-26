# define a named vector using vector names

define a named vector using vector names

## Usage

``` r
nameVectorN(x, makeNamesFunc = makeNames, ...)
```

## Arguments

- x:

  `character` vector or any object which has names available `names(x)`.

- makeNamesFunc:

  `function` used to create unique names, in the event that the names(x)
  are not unique.

- ...:

  Additional arguments are ignored.

## Value

vector of names, whose names are uniquely assigned using
[`makeNames`](https://jmw86069.github.io/jamba/reference/makeNames.md)
using the values of the vector.

## Details

This function creates a vector from the names of the input vector, then
assigns the same as names. The utility is mainly for
[`lapply`](https://rdrr.io/r/base/lapply.html) functions which maintain
the name of a vector in its output. The reason to run
[`lapply`](https://rdrr.io/r/base/lapply.html) using names is so the
lapply function is operating only on the name and not the data it
references, which can be convenient when the name of the element is
useful to known inside the function body. The reason to name the names,
is so the list object returned by
[`lapply`](https://rdrr.io/r/base/lapply.html) is also named with these
same consistent names.

Consider a list of data.frames, each of which represents stats results
from a contrast and fold change. The data.frame may not indicate the
name of the contrast, while the list itself may be named by the
contrast. One would `lapply(nameVectorN(listDF), function(iName)iName)`
which allows the internal function access to the name of each list
element. This could for example be added to the data.frame.

## See also

Other jam string functions:
[`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md),
[`breaksByVector()`](https://jmw86069.github.io/jamba/reference/breaksByVector.md),
[`fillBlanks()`](https://jmw86069.github.io/jamba/reference/fillBlanks.md),
[`formatInt()`](https://jmw86069.github.io/jamba/reference/formatInt.md),
[`gsubOrdered()`](https://jmw86069.github.io/jamba/reference/gsubOrdered.md),
[`gsubs()`](https://jmw86069.github.io/jamba/reference/gsubs.md),
[`makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md),
[`nameVector()`](https://jmw86069.github.io/jamba/reference/nameVector.md),
[`padInteger()`](https://jmw86069.github.io/jamba/reference/padInteger.md),
[`padString()`](https://jmw86069.github.io/jamba/reference/padString.md),
[`pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md),
[`pasteByRowOrdered()`](https://jmw86069.github.io/jamba/reference/pasteByRowOrdered.md),
[`sizeAsNum()`](https://jmw86069.github.io/jamba/reference/sizeAsNum.md),
[`tcount()`](https://jmw86069.github.io/jamba/reference/tcount.md),
[`ucfirst()`](https://jmw86069.github.io/jamba/reference/ucfirst.md)

## Examples

``` r
# a simple integer vector with character names
L <- nameVector(1:5, LETTERS[1:5]);
L;
#> A B C D E 
#> 1 2 3 4 5 

# we can make a vector of names, retaining the names
nameVectorN(L);
#>   A   B   C   D   E 
#> "A" "B" "C" "D" "E" 

# Now consider a named list, where the name is important
# to keep for downstream work.
K <- list(A=(1:3)^3, B=7:10, C=(1:4)^2);
K;
#> $A
#> [1]  1  8 27
#> 
#> $B
#> [1]  7  8  9 10
#> 
#> $C
#> [1]  1  4  9 16
#> 
# Typical lapply-style work does not operate on the name,
# making it difficult to use the name inside the function.
# Here, we just add the name to the colnames, but anything
# could be useful.
lapply(K, function(i){
    data.frame(mean=mean(i), median=stats::median(i));
 });
#> $A
#>   mean median
#> 1   12      8
#> 
#> $B
#>   mean median
#> 1  8.5    8.5
#> 
#> $C
#>   mean median
#> 1  7.5    6.5
#> 

# So the next step is to run lapply() on the names
lapply(names(K), function(i){
   iDF <- data.frame(mean=mean(K[[i]]), median=stats::median(K[[i]]));
   colnames(iDF) <- paste(c("mean", "median"), i);
   iDF;
})
#> [[1]]
#>   mean A median A
#> 1     12        8
#> 
#> [[2]]
#>   mean B median B
#> 1    8.5      8.5
#> 
#> [[3]]
#>   mean C median C
#> 1    7.5      6.5
#> 
# The result is good, but the list is no longer named.
# The nameVectorN() function is helpful for maintaining the names.

# So we run lapply() on the named-names, which keeps the names in
# the resulting list, and sends it into the function.
lapply(nameVectorN(K), function(i){
   iDF <- data.frame(mean=mean(K[[i]]), median=stats::median(K[[i]]));
   colnames(iDF) <- paste(c("mean", "median"), i);
   iDF;
});
#> $A
#>   mean A median A
#> 1     12        8
#> 
#> $B
#>   mean B median B
#> 1    8.5      8.5
#> 
#> $C
#>   mean C median C
#> 1    7.5      6.5
#> 
```
