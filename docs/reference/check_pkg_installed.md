# Lightweight method to check if an R package is installed

Lightweight method to check if an R package is installed

## Usage

``` r
check_pkg_installed(x, useMethod = c("packagedir", "requireNamespace"), ...)
```

## Arguments

- x:

  `character` string of package or packages to test.

- useMethod:

  `character` default "packagedir" with the method of package
  confirmation.

  - "packagedir" provides a rapid test for the presence of an R package,
    without loading the package namespace. It tests whether
    `system.file(package=x)` returns a non-empty value, then
    'DESCRIPTION' file exists in the package directory. It answers the
    question: "Is 'x' package installed?" It does not answer: "Is 'x'
    package usable in the current R session?" When `useMethod` also
    includes "requireNamespace", for any FALSE result it will also
    perform a secondary check as well, to confirm the package cannot be
    loaded by another mechanism.

  - "requireNamespace" uses
    [`requireNamespace(x, quietly=TRUE)`](https://rdrr.io/r/base/ns-load.html),
    with slight benefit that it accepts multiple values for `x`, and
    returns the result without using
    [`invisible()`](https://rdrr.io/r/base/invisible.html). This method
    loads the package namespace, but does not attach it. This method
    therefore takes the same time as loading the package, in return for
    providing the most accurate answer to the question: "Is 'x' package
    usable by this R session right now?"

- ...:

  additional arguments are ignored.

## Value

`logical` vector indicating whether each value in `x` represents an
installed R package. The vector is named by packages provided in `x`.

## Details

There are many methods to test for an installed package. Most approaches
incur some time or resource penalty, so `check_pkg_installed()` is
motivated for rapid results without loading the package namespace.

This function also accepts multiple values for `x` for convenience.

There are two available methods defined by `useMethod`:

1.  `useMethod="packagedir"` confirms: this function represents possibly
    the most gentle and rapid approach. It simply calls
    `system.file(package=x)`, for each entry of `x`, then checks these
    requirements:

    - Does the package directory exist via `system.file(package=x)`

    - Does the package directory contain the file 'DESCRIPTION'?

    - It does not check whether the package can be loaded into the
      current R session.

2.  `useMethod="requireNamespace"` confirms:

    - [`requireNamespace(x, quietly=TRUE)`](https://rdrr.io/r/base/ns-load.html)
      returns TRUE

    - It therefore loads the package namespace to confirm, but does not
      attach the package to the current session. It therefore may take
      time and resources, despite not altering the R environment search
      path.

The default behavior first tests by "packagedir", then for any `FALSE`
results it also tests `"requireNamespace"`.

## See also

Other jam practical functions:
[`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md),
[`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md),
[`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md),
[`colNum2excelName()`](https://jmw86069.github.io/jamba/reference/colNum2excelName.md),
[`color_dither()`](https://jmw86069.github.io/jamba/reference/color_dither.md),
[`exp2signed()`](https://jmw86069.github.io/jamba/reference/exp2signed.md),
[`getAxisLabel()`](https://jmw86069.github.io/jamba/reference/getAxisLabel.md),
[`isFALSEV()`](https://jmw86069.github.io/jamba/reference/isFALSEV.md),
[`isTRUEV()`](https://jmw86069.github.io/jamba/reference/isTRUEV.md),
[`jargs()`](https://jmw86069.github.io/jamba/reference/jargs.md),
[`kable_coloring()`](https://jmw86069.github.io/jamba/reference/kable_coloring.md),
[`lldf()`](https://jmw86069.github.io/jamba/reference/lldf.md),
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
check_pkg_installed("methods")
#> methods 
#>    TRUE 

check_pkg_installed(c("jamba",
   "multienrichjam",
   "venndir",
   "methods",
   "blah"))
#>          jamba multienrichjam        venndir        methods           blah 
#>           TRUE           TRUE           TRUE           TRUE          FALSE 
```
