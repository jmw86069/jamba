# Smooth scatter plot, Jam style

Produce smooth scatter plot, a helper function called by
[`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md).

## Usage

``` r
smoothScatterJam(
  x,
  y = NULL,
  nbin = 256,
  bandwidth,
  colramp = grDevices::colorRampPalette(c("white", "lightblue", "blue", "orange",
    "orangered2")),
  nrpoints = 100,
  pch = ".",
  cex = 1,
  col = "black",
  transformation = function(x) x^0.25,
  postPlotHook = graphics::box,
  xlab = NULL,
  ylab = NULL,
  xlim,
  ylim,
  add = FALSE,
  xaxs = graphics::par("xaxs"),
  yaxs = graphics::par("yaxs"),
  xaxt = graphics::par("xaxt"),
  yaxt = graphics::par("yaxt"),
  useRaster = NULL,
  ...
)
```

## Arguments

- x:

  `numeric` vector, or data matrix with two or more columns.

- y:

  `numeric` vector, or if data is supplied via x as a matrix, y is NULL.

- nbin:

  `integer` number of bins to use when converting the kernel density
  result (which uses bandwidthN above) into a usable image. For example,
  nbin=123 is the default used by
  [`graphics::smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html),
  however the
  [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  function default is higher (256).

- bandwidth:

  `numeric` vector used to define the y- and x-axis bandwidths,
  respectively, passed to
  [`KernSmooth::bkde2D()`](https://rdrr.io/pkg/KernSmooth/man/bkde2D.html),
  which calculates the underlying 2-dimensional kernel density. The
  [`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
  function was motivated by never wanting to define this number
  directly, instead auto-calculation suitable values.

- colramp:

  `function` that takes one `numeric` argument and returns that integer
  number of colors, by default 256.

- nrpoints:

  `integer` number of outlier datapoints to display, as defined by
  [`graphics::smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html),
  however the default here is `nrpoints=0` to avoid additional clutter
  in the output, and because the default argument `bandwidthN` usually
  indicates all individual points.

- pch:

  `integer` point shape used when `nrpoints>0`.

- cex:

  `numeric` point size expansion factor used when `nrpoints>0`.

- col:

  `character` R color used when `nrpoints>0`.

- transformation:

  `function` which converts point density to a number, typically related
  to square root or cube root transformation.

- postPlotHook:

  `function` or 'NULL', NULL default. When `function` is supplied, it is
  called after producing the image. By default it is simply used to draw
  a box around the image, but could be used to layer additional
  information atop the image plot, for example contours, labels, etc.

- xlab:

  `character` x-axis label

- ylab:

  `character` y-axis label

- xlim:

  `numeric` x-axis range for the plot

- ylim:

  `numeric` y-axis range for the plot

- add:

  `logical` whether to add to an existing active R plot, or create a new
  plot window.

- xaxs:

  `character` value compatible with `graphics::par("xaxs")`, mainly
  useful for suppressing the x-axis, in order to produce a custom x-axis
  range, most useful to restrict the axis range expansion done by R by
  default.

- yaxs:

  `character` value compatible with `graphics::par("yaxs")`, mainly
  useful for suppressing the y-axis, in order to produce a custom y-axis
  range, most useful to restrict the axis range expansion done by R by
  default.

- xaxt:

  `character` value compatible with `graphics::par("xaxt")`, mainly
  useful for suppressing the x-axis, in order to produce a custom x-axis
  by other mechanisms, e.g. log-scaled x-axis tick marks.

- yaxt:

  `character` value compatible with `graphics::par("yaxt")`, mainly
  useful for suppressing the y-axis, in order to produce a custom y-axis
  by other mechanisms, e.g. log-scaled y-axis tick marks.

- useRaster:

  'NULL' or `logical` indicating whether to invoke
  [`graphics::rasterImage()`](https://rdrr.io/r/graphics/rasterImage.html)
  to produce a raster image. If NULL, it determines whether to produce a
  raster image within the
  [`imageDefault()`](https://jmw86069.github.io/jamba/reference/imageDefault.md)
  function, which checks the options using
  `getOption("preferRaster", FALSE)` to determine among other things,
  whether the user prefers raster images, and if the
  [`grDevices::dev.capabilities()`](https://rdrr.io/r/grDevices/dev.capabilities.html)
  supports raster.

- ...:

  additional arguments are passed to
  [`imageDefault()`](https://jmw86069.github.io/jamba/reference/imageDefault.md)
  and optionally to `plotPlotHook()` when supplied.

## Value

`list` of elements sufficient to call
[`graphics::image()`](https://rdrr.io/r/graphics/image.html), also by
default this function is called for the byproduct of creating a figure.

## Details

For general purposes, use
[`plotSmoothScatter()`](https://jmw86069.github.io/jamba/reference/plotSmoothScatter.md)
as a replacement for
[`graphics::smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html),
which produces better default settings for pixel size and density
bandwidth.

This function is only necessary in order to override the
[`graphics::smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html)
function which calls
[`graphics::image.default()`](https://rdrr.io/r/graphics/image.html).
Instead, this function calls
[`imageDefault()`](https://jmw86069.github.io/jamba/reference/imageDefault.md)
which is required in order to utilize custom raster image scaling,
particularly important when the x- and y-axis ranges are not similar,
e.g. where the x-axis spans 10 units, but the y-axis spans 10,000 units.

## See also

[`graphics::smoothScatter()`](https://rdrr.io/r/graphics/smoothScatter.html)

Other jam internal functions:
[`handleArgsText()`](https://jmw86069.github.io/jamba/reference/handleArgsText.md),
[`jamCalcDensity()`](https://jmw86069.github.io/jamba/reference/jamCalcDensity.md),
[`make_html_styles()`](https://jmw86069.github.io/jamba/reference/make_html_styles.md),
[`make_styles()`](https://jmw86069.github.io/jamba/reference/make_styles.md)

## Examples

``` r
x1 <- rnorm(1000);
y1 <- (x1 + 5)* 4  + rnorm(1000);
smoothScatterJam(x=x1, y=y1, bandwidth=c(0.05, 0.3))

```
