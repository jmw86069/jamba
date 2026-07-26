# Long listing of R session objects

Long listing of R session objects

## Usage

``` r
lldf(
  n = Inf,
  envir = -1L,
  items = NULL,
  use_utils_objectsize = TRUE,
  all.names = TRUE,
  ...
)
```

## Arguments

- n:

  `integer` or `Inf` indicating how many objects to include in the
  output `data.frame`.

- envir:

  `environment` where the list of objects is obtained, default `-1L`
  searches the environment of the caller, usually the user workspace.
  Other recognized options:

  - `character` string suitable for
    [`as.environment()`](https://rdrr.io/r/base/as.environment.html)
    which recognizes the search path returned by
    [`search()`](https://rdrr.io/r/base/search.html)

  - `integer` or `numeric` equivalent to environment relative position
    as used in [`ls()`](https://rdrr.io/r/base/ls.html) argument `pos`.

- items:

  `character` of items to include, default NULL.

- use_utils_objectsize:

  `logical`, default TRUE, whether to prefer
  [`utils::object.size()`](https://rdrr.io/r/utils/object.size.html),
  otherwise it will attempt to use `pryr::object_size()` if the package
  is installed.

- all.names:

  `logical` passed to [`base::ls()`](https://rdrr.io/r/base/ls.html)
  indicating whether to include all names, where `all.names=TRUE` will
  include hidden objects whose name begin with `"."` such as `".First"`.

- ...:

  additional arguments are passed to
  [`ls()`](https://rdrr.io/r/base/ls.html), notably `pattern` can be
  passed to subset objects by regular expression.

## Value

`data.frame` with summary of objects and object sizes, sorted by
decreasing object size.

## Details

This function expands [`base::ls()`](https://rdrr.io/r/base/ls.html) by
also determining the object size, and sorting to display the top `n`
objects by size, largest first.

This package will call `pryr::object_size` if available, otherwise falls
back to
[`utils::object.size()`](https://rdrr.io/r/utils/object.size.html).

## See also

Other jam practical functions:
[`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md),
[`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md),
[`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md),
[`check_pkg_installed()`](https://jmw86069.github.io/jamba/reference/check_pkg_installed.md),
[`colNum2excelName()`](https://jmw86069.github.io/jamba/reference/colNum2excelName.md),
[`color_dither()`](https://jmw86069.github.io/jamba/reference/color_dither.md),
[`exp2signed()`](https://jmw86069.github.io/jamba/reference/exp2signed.md),
[`getAxisLabel()`](https://jmw86069.github.io/jamba/reference/getAxisLabel.md),
[`isFALSEV()`](https://jmw86069.github.io/jamba/reference/isFALSEV.md),
[`isTRUEV()`](https://jmw86069.github.io/jamba/reference/isTRUEV.md),
[`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md),
[`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md),
[`log2signed()`](https://jmw86069.github.io/jamba/reference/log2signed.md),
[`middle()`](https://jmw86069.github.io/jamba/reference/middle.md),
[`minorLogTicks()`](https://jmw86069.github.io/jamba/reference/minorLogTicks.md),
[`newestFile()`](https://jmw86069.github.io/jamba/reference/newestFile.md),
[`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md),
[`reload_qmd_cache()`](https://jmw86069.github.io/jamba/reference/reload_qmd_cache.md),
[`reload_rmarkdown_cache()`](https://jmw86069.github.io/jamba/reference/reload_rmarkdown_cache.md),
[`renameColumn()`](https://jmw86069.github.io/jamba/reference/renameColumn.md),
[`rmInfinite()`](https://jmw86069.github.io/jamba/reference/rmInfinite.md),
[`rmNA()`](https://jmw86069.github.io/jamba/reference/rmNA.md),
[`rmNAs()`](https://jmw86069.github.io/jamba/reference/rmNAs.md),
[`rmNULL()`](https://jmw86069.github.io/jamba/reference/rmNULL.md),
[`setPrompt()`](https://jmw86069.github.io/jamba/reference/setPrompt.md)

## Examples

``` r
lldf(10);
#> $size
#> character(0)
#> 

# custom environment
newenv <- new.env();
newenv$A <- 1:10;
newenv$df <- data.frame(A=1:10, B=11:20);
lldf(envir=newenv);
#>    name      class bytes      size
#> df   df data.frame   944 944 bytes
#> A     A    integer    96  96 bytes
rm(newenv);
```
