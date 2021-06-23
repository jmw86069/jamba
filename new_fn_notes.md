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


## colortable()

A long-awaited port of a function I previously used all the time but
have not properly ported into an R package, and so I miss using it.
*sad face*

* It trims displayed column width so all columns fit the console width
* Columns are numbered, rows are numbered.
* Highlighted `colnames` and `rownames`
* Row names are highlighted, encouraged, and warmly embraced. (Sorry `tibble`!)
* Numeric values are formatted for friendly output:

   * values above `1000` use `big.mark=","` to appear like this: `1,000`
   * decimal values use reasonable significant digits, so instead of
   displaying `3.141592653589793116` it will display `3.14`.
   * however columns with values `c(1000, 0.001)` will be displayed
   `c("1,000", "0.001")` - and not the default which is to apply the
   same level of detail to every value, for example
   not this: `c("1e+03", "1e-03")`, and not this: `c("1,000.000", "0.001")`
   * P-values may have specific formatting rules, I forget the details rn.

* If not all columns will fit the console width when shrunk down to some
a minimum width, it splits output across multiple lines, and
includes row name and row number beside each split.

   * For me, it is not helpful to print only the first `n` columns, the
   main purpose is to see every column.
   * For me, it is not helpful to split super-wide columns across multiple lines,
   sometimes this process splits into one chunk per column. For me
   it is much more helpful to shorten the displayed value in each cell.
   * There is some logic to determine a reasonable size to shrink each
   column, for example `nchar()` upper quartile.

* Possibly the most useful feature is that values in each column are colorized.
This step helps recognize outliers, typos, inconsistencies, etc.
* Each column is colorized based upon its data type:

   * `character` columns are colorized using supplied `color_sub` for
   name:color substitution, otherwise it calls `colorjam::rainbowJam()`.
   * `numeric` columns are colorized one of a few ways:
   
      * Default: divergent blue-white-red colors using `colorjam::col_div_xf()`
      extended to the maximum absolute value, linear scale. Option to apply
      quantile or percent-max logic to redefine the maximum value. Option
      to apply in log scale using `jamba::log2signed()`.
      * P-values: colorized using `-log10(x)` which emphasizes P-values
      near zero, and therefore values near 1 are not colorized.
      Ideally, P-value columns are recognized by colname and/or by the numeric values.
      * Correlation: Same as Default but uses a fixed maximum=1 for consistency.
      * Global and per-column max values can be defined, to help apply
      a useful color gradient.

* Columns can be sorted, by proxy of calling `jamba::mixedSortDF()` to
enable mixed alphanumeric sorting.
* Features intended but not enabled (yet):

   * optional word-wrapping column headers
   * optional word-wrapping character values, for up to `n` lines,
   expanding the number of lines used for affected rows.
   * Ability to view proper output from `table()`, and not the mangled
   tall-skinny format from `as.data.frame(table(x))`
   * Ability to view output from `table()` with more than two variables.
   * Ability to view output from n-dimensional arrays.


