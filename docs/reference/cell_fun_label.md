# ComplexHeatmap cell function to label heatmap cells

ComplexHeatmap cell function to label heatmap cells

## Usage

``` r
cell_fun_label(
  m,
  prefix = "",
  suffix = "",
  cex = 1,
  col_hm = NULL,
  outline = FALSE,
  abbrev = FALSE,
  show = NULL,
  rot = 0,
  sep = "\n",
  verbose = FALSE,
  ...
)
```

## Arguments

- m:

  `numeric` matrix or `list` of `matrix` objects. The first `matrix`
  object must be `numeric` and compatible with the color function
  `col_hm`.

- prefix, suffix:

  `character` vectors that define a prefix and suffix for each value in
  `m` for each cell.

- cex:

  `numeric` adjustment for the fontsize used for each label, which is
  multiplied by the default `fontsize=10` to determine the fontsize.

- col_hm:

  `function` as returned by
  [`circlize::colorRamp2()`](https://rdrr.io/pkg/circlize/man/colorRamp2.html)
  which should be the same function used to create the heatmap

- outline:

  `logical` indicating whether to draw an outline around each heatmap
  cell

- abbrev:

  `logical` indicating whether numeric values should be abbreviated
  using `jamba::asSize(..., kiloSize=1000)` which effectively reduces
  large numbers to `k` for thousands, `M` for millions (M for Mega), `G`
  for billions (G for Giga), etc.

- show:

  `integer` used when `m` is supplied as a `list` of matrices, in which
  case `show` is used to define which values should be used as cell
  labels. By default, all matrices are used.

- rot:

  `numeric` value used to rotate cell label text, default 0 is
  horizontal.

- sep:

  `character` string, default `"\n"` newline, used when there are
  multiple labels per cell, which also requires `m` as a list, and
  `show` is 'NULL' or has multiple values.

- verbose:

  `logical` indicating whether to print verbose output, specifically
  printing label information for position `(1, 1)`. This output will
  only be seen when rendering or building the Heatmap object.

- ...:

  additional arguments are ignored.

## Value

`function` sufficient to use as input to
[`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html)
argument `cell_fun`.

## Details

This function serves as a convenient method to add text labels to each
cell in a heatmap produced by
[`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html),
via the argument `cell_fun`.

Note that this function requires re-using the specific color function
used for the heatmap in the call to
[`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html).

This function is slightly unique in that it allows multiple labels, if
`m` is supplied as a `list` of `matrix` objects. In fact, some `matrix`
objects may contain `character` values with custom labels.

Cell labels are colored based upon the heatmap cell color, which is
passed to
[`jamba::setTextContrastColor()`](https://jmw86069.github.io/jamba/reference/setTextContrastColor.md)
to determine whether to use light or dark text color for optimum
contrast.

TODO: Option to supply a `logical` matrix to define a subset of cells to
label, for example only labels that meet a filter criteria.
Alternatively, the matrix data supplied in `m` can already be filtered.

TODO: Allow some matrix values that contain `character` data to use
`gridtext` for custom markdown formatting. That process requires a
slightly different method.

## See also

Other jam heatmap functions:
[`heatmap_column_order()`](https://jmw86069.github.io/jamba/reference/heatmap_column_order.md),
[`heatmap_row_order()`](https://jmw86069.github.io/jamba/reference/heatmap_row_order.md)

## Examples

``` r

m <- matrix(stats::rnorm(16)*2, ncol=4)
colnames(m) <- LETTERS[1:4]
rownames(m) <- letters[1:4]
col_hm <- circlize::colorRamp2(breaks=(-2:2) * 2,
   colors=c("navy", "dodgerblue", "white", "tomato", "red4"))

# the heatmap can be created in one step
hm <- ComplexHeatmap::Heatmap(m,
   col=col_hm,
   heatmap_legend_param=list(
      color_bar="discrete",
      border=TRUE,
      at=-4:4),
   cell_fun=cell_fun_label(m,
      col_hm=col_hm))
ComplexHeatmap::draw(hm)


# the cell label function can be created first
cell_fun <- cell_fun_label(m,
   outline=TRUE,
   cex=1.5,
   col_hm=col_hm)
hm2 <- ComplexHeatmap::Heatmap(m,
   col=col_hm,
   cell_fun=cell_fun)
ComplexHeatmap::draw(hm2)
```
