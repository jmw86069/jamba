# check lightMode for light background color

check lightMode for light background color

## Usage

``` r
checkLightMode(
  lightMode = getOption("jam.lightMode"),
  positron_default = getOption("jam.lightMode.positron", TRUE),
  html_default = getOption("jam.lightMode.html", TRUE),
  knitr_default = getOption("jam.lightMode.knitr", TRUE),
  other_default = getOption("jam.lightMode.other", FALSE),
  ...
)
```

## Arguments

- lightMode:

  `logical` or NULL, indicating whether the lightMode parameter has been
  defined in the function call. The default `getOption("jam.lightMode")`
  is NULL unless assigned. When NULL it will use logic described in
  Details. When assigned, it is used as-is.

- positron_default:

  `logical` default TRUE, applied only when running inside Positron, and
  when
  [`rstudioapi::getThemeInfo()`](https://rstudio.github.io/rstudioapi/reference/getThemeInfo.html)
  is not available, which is default for Positron as of jamba-1.0.5.
  When running inside RStudio, it calls
  [`rstudioapi::getThemeInfo()`](https://rstudio.github.io/rstudioapi/reference/getThemeInfo.html)
  to determine whether the theme is light or dark.

- html_default:

  `logical` default TRUE, applied when knitr is running with html
  output.

- knitr_default:

  `logical` default TRUE, applied when knitr is running.

- other_default:

  `logical` default FALSE, applied when running outside Positron or
  RStudio.

- ...:

  Additional arguments are ignored.

## Value

`logical` indicating whether lightMode is defined

## Details

Check the lightMode status through options('jam.lightMode'), or by
determining the running environment.

Logic is applied as follows:

- When `lightMode` is defined upfront as `logical`, it is used.

- When `getOption('jam.lightMode')` is defined, it is used.

- When `lightMode` is NULL, it will apply a suitable default:

  - If running inside knitr, it will apply `html_default` for html
    output, and `knitr_default` otherwise. Typically both produce white
    background, however this gives the option for custom background
    color.

  - If running inside Positron or RStudio, it will attempt to run
    [`rstudioapi::getThemeInfo()`](https://rstudio.github.io/rstudioapi/reference/getThemeInfo.html)
    and use the resulting 'theme\$dark', otherwise it applies
    `positron_default`.

  - All other cases use `other_default`, default is FALSE to apply to
    linux remote shell session with dark background.

To set a persistent default lightMode, add this line to .Rprofile:

- `options("jam.lightMode"=TRUE)`

## See also

Other jam practical functions:
[`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md),
[`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md),
[`check_pkg_installed()`](https://jmw86069.github.io/jamba/reference/check_pkg_installed.md),
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
checkLightMode(FALSE);
#> [1] FALSE
checkLightMode();
#> [1] TRUE
```
