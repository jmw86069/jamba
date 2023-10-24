
# Prepare for CRAN release

### Function documentation

* All functions must contain `@returns`
* Most or all functions should contain `@examples`


### Unit tests

* All or most key functions should include unit tests for basic functionality.


### Code changes for CRAN compliance

* DONE. Remove use of `:::`, replace with call to exported function,
or internal function that performs equivalent task.

   * DONE: `grDevices:::.smoothScatterCalcDensity()` - is a wrapper for
   `KernSmooth::bkde2D()` so can be replaced with internal Jam function.
   * DONE: `RColorBrewer:::namelist` - should be removed, RColorBrewer is not
   in Imports, but in Suggests. Use manually-encoded method to recognize
   valid RColorBrewer color names. (`jamba-showcolors.R:52`)
   * DONE: `utils:::relist.default()` - remove reference in help docs, see
   `jamba-string.r:2411`.
   * DONE: `base:::merge.data.frame()` - manually recognize argument names,
   see `jamba.r:4632`.

* Consider splitting large files into smaller counterparts, examples:

   * `jamba.r`: 5,074 lines
   * `jamba-plots.r`:      3,341 lines
   * `jamba-string.r`:     2,629 lines
   * `jamba-colors.r`:     2,107 lines
   * `jamba-export.r`:     1,762 lines
   * `jamba-mixedSort.R`:  1,147 lines

* Port `imageDefault()` to call `image.default()`

   * DONE: avoid `.External.graphics(graphics:::C_image, x, y, zi, col)`
