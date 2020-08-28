# New function notes for jamba

* `grepls1()` - easy extension of `grepls(..., 1)` which searches only the 
user environment.

## Text-centric, or console-centric

* `plotSmoothScatterText()` - text version of `plotSmoothScatter()`
that uses package `"nostalgiR"` enhanced with density per point.
Optional ability to tile multiple plot panels, stopping short of
replicating ggplot2.
* `imageByColorsText()` - text version of `imageByColors()` which
takes a matrix of colors, a matrix of labels, and displays it as
an image. Bonus points for centering text labels across multiple
neighboring cells with same color and label.
