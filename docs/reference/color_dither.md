# Make dithered color pattern light-dark

Make dithered color pattern light-dark

## Usage

``` r
color_dither(
  x,
  L_diff = 4,
  L_max = 90,
  L_min = 30,
  min_contrast = 1.25,
  direction = 1,
  returnType = c("vector", "list", "matrix"),
  debug = FALSE,
  ...
)
```

## Arguments

- x:

  `character` vector of R colors

- L_diff:

  `numeric` value added or subtracted from the L in HSL color space for
  each color, until contrast is at least `min_contrast`.

- L_max, L_min:

  `numeric` values that define the permitted range of L values in HSL
  color space, which ranges from 0 to 100.

- min_contrast:

  `numeric` minimum contrast as defined by
  [`colorspace::contrast_ratio()`](https://colorspace.R-Forge.R-project.org/reference/contrast_ratio.html)
  for the input and potential output color.

- direction:

  `numeric` that defines the initial direction, where values \>= 0 start
  by making colors lighter, and values \< 0 make colors darker.

- returnType:

  `character` string that defines the output of this function:

  - `vector`: two colors for every input color in `x`

  - `matrix`: two rows, input colors on first row, output colors on
    second row

  - `list`: a `list` with two colors in each element, with input and
    output colors together in each vector.

- debug:

  `logical` indicating whether to plot the color iterations using
  [`showColors()`](https://jmw86069.github.io/jamba/reference/showColors.md).

- ...:

  additional arguments are ignored.

## Value

format defined by argument `returnType`:

- `vector`: two colors for every input color in `x`

- `matrix`: two rows, input colors on first row, output colors on second
  row

- `list`: a `list` with two colors in each element, with input and
  output colors together in each vector.

## Details

This function serves a very simple purpose, mainly for
[`printDebug()`](https://jmw86069.github.io/jamba/reference/printDebug.md)
to use subtle alternating light/dark colors for vector output. It takes
a color and returns two colors which are slightly lighter and darker
than each other, to a minimum contrast defined by
[`colorspace::contrast_ratio()`](https://colorspace.R-Forge.R-project.org/reference/contrast_ratio.html).

## See also

Other jam practical functions:
[`breakDensity()`](https://jmw86069.github.io/jamba/reference/breakDensity.md),
[`call_fn_ellipsis()`](https://jmw86069.github.io/jamba/reference/call_fn_ellipsis.md),
[`checkLightMode()`](https://jmw86069.github.io/jamba/reference/checkLightMode.md),
[`check_pkg_installed()`](https://jmw86069.github.io/jamba/reference/check_pkg_installed.md),
[`colNum2excelName()`](https://jmw86069.github.io/jamba/reference/colNum2excelName.md),
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
x <- "firebrick1";
showColors(color_dither(x))


showColors(color_dither(x, direction=-1))


x <- vigrep("^green[0-9]", grDevices::colors())
showColors(color_dither(x))

showColors(color_dither(x, direction=-1, returnType="list"))


x <- c("green1", "cyan", "blue", "red", "gold", "yellow", "pink")
showColors(color_dither(x))


color_dither(x, debug=TRUE)

#>  [1] "green1"  "#00D600" "cyan"    "#00D6D6" "blue"    "#3D3DFF" "red"    
#>  [8] "#FF5252" "gold"    "#FFF4B8" "yellow"  "#D6D600" "pink"    "#FF97A9"
```
