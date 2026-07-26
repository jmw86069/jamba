# break a vector into groups

breaks a vector into groups

## Usage

``` r
breaksByVector(x, labels = NULL, returnFractions = FALSE, ...)
```

## Arguments

- x:

  `character` vector of labels

- labels:

  `character` vector of custom labels to represent the items in x

- returnFractions:

  `logical` whether to return fractional coordinates for labels that
  should be positioned between two labels

- ...:

  additional parameters are ignored.

## Value

`list` with the following named elements:

- `"breakPoints"`: The mid-point coordinate between each break. These
  midpoints would be good for drawing dividing lines for example.

- `"labelPoints"`: The ideal point to place a label to represent the
  group.

- `"newLabels"`: A vector of labels the same length as the input data,
  except using blank values except where a label should be drawn. This
  output is good for text display.

- `"useLabels"`: The unique set of labels, without blanks, corresponding
  to the coordinates supplied by labelPoints.

- `"breakLengths"`: The integer size of each set of labels.

## Details

This function takes a vector of values, determines "chunks" of identical
values, from which it defines where breaks occur. It assumes the input
vector is ordered in the way it will be displayed, with some labels
being duplicated consecutively. This function defines the breakpoints
where the labels change, and returns the ideal position to put a single
label to represent a duplicated consecutive set of labels.

It can return fractional coordinates, for example when a label
represents two consecutive items, the fractional coordinate can be used
to place the label between the two items.

This function is useful for things like adding labels to
[`imageDefault()`](https://jmw86069.github.io/jamba/reference/imageDefault.md)
color image map of sample groupings, where it may be ideal to label only
unique elements in a contiguous set.

## See also

Other jam string functions:
[`asSize()`](https://jmw86069.github.io/jamba/reference/asSize.md),
[`fillBlanks()`](https://jmw86069.github.io/jamba/reference/fillBlanks.md),
[`formatInt()`](https://jmw86069.github.io/jamba/reference/formatInt.md),
[`gsubOrdered()`](https://jmw86069.github.io/jamba/reference/gsubOrdered.md),
[`gsubs()`](https://jmw86069.github.io/jamba/reference/gsubs.md),
[`makeNames()`](https://jmw86069.github.io/jamba/reference/makeNames.md),
[`nameVector()`](https://jmw86069.github.io/jamba/reference/nameVector.md),
[`nameVectorN()`](https://jmw86069.github.io/jamba/reference/nameVectorN.md),
[`padInteger()`](https://jmw86069.github.io/jamba/reference/padInteger.md),
[`padString()`](https://jmw86069.github.io/jamba/reference/padString.md),
[`pasteByRow()`](https://jmw86069.github.io/jamba/reference/pasteByRow.md),
[`pasteByRowOrdered()`](https://jmw86069.github.io/jamba/reference/pasteByRowOrdered.md),
[`sizeAsNum()`](https://jmw86069.github.io/jamba/reference/sizeAsNum.md),
[`tcount()`](https://jmw86069.github.io/jamba/reference/tcount.md),
[`ucfirst()`](https://jmw86069.github.io/jamba/reference/ucfirst.md)

## Examples

``` r
b <- rep(LETTERS[c(1:5, 1)], c(2,3,5,4,3,4));
bb <- breaksByVector(b);
# Example showing how labels can be minimized inside a data.frame
data.frame(b,
   newLabels=bb$newLabels);
#>    b newLabels
#> 1  A         A
#> 2  A          
#> 3  B          
#> 4  B         B
#> 5  B          
#> 6  C          
#> 7  C          
#> 8  C         C
#> 9  C          
#> 10 C          
#> 11 D          
#> 12 D         D
#> 13 D          
#> 14 D          
#> 15 E          
#> 16 E         E
#> 17 E          
#> 18 A          
#> 19 A         A
#> 20 A          
#> 21 A          

# Example showing how to reposition text labels
# so duplicated labels are displayed in the middle
# of each group
bb2 <- breaksByVector(b, returnFractions=TRUE);
ylabs <- c("minimal labels", "all labels");
withr::with_par(adjustAxisLabelMargins(ylabs, 2), {
   withr::local_par(adjustAxisLabelMargins(bb2$useLabels, 1))
   nullPlot(xlim=range(seq_along(b)), ylim=c(0,3),
      doBoxes=FALSE, doUsrBox=TRUE);
   graphics::axis(2, las=2, at=c(1,2), ylabs);
   graphics::text(y=2, x=seq_along(b), b);
   graphics::text(y=1, x=bb2$labelPoints, bb2$useLabels);

## Print axis labels in the center of each group
graphics::axis(3,
   las=2,
   at=bb2$labelPoints,
   labels=bb2$useLabels);

## indicate each region
for (i in seq_along(bb2$breakPoints)) {
   graphics::axis(1,
      at=c(c(0, bb2$breakPoints)[i]+0.8, bb2$breakPoints[i]+0.2),
      labels=c("", ""));
}
## place the label centered in each region without adding tick marks
graphics::axis(1,
   las=2,
   tick=FALSE,
   at=bb2$labelPoints,
   labels=bb2$useLabels);
## abline to indicate the boundaries, if needed
graphics::abline(v=c(0, bb2$breakPoints) + 0.5,
   lty="dashed",
   col="blue");

})

# The same process is used by imageByColors()
```
