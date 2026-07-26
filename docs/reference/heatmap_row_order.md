# Return Heatmap row order from ComplexHeatmap heatmap object

Return Heatmap row order from ComplexHeatmap heatmap object

## Usage

``` r
heatmap_row_order(hm, which_heatmap = NULL)
```

## Arguments

- hm:

  `Heatmap` or `HeatmapList` object as defined by the Bioconductor
  package via
  [`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html).

- which_heatmap:

  `integer`, default NULL, used when the input is a `HeatmapList` with
  multiple heatmaps.

## Value

output depends upon the heatmap:

- When heatmap rows are grouped using `row_split`, and when the data
  matrix contains rownames, returns a `character` vector of rownames in
  the order they appear in the heatmap. When there are no rownames,
  `integer` row index values are returned. If the heatmap has row
  labels, they are returned as vector names.

- When rows are grouped using `row_split`, it returns a `list` of
  vectors as described above. The `list` is named using the `row_title`
  labels only when there is an equal number of row labels.

## Details

This function is a helpful utility to return the fully qualified list of
rownames in a
[`ComplexHeatmap::Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html)
object.

The core intention is for the output to be usable with the original data
matrix used in the heatmap. Therefore, the vector values are
[`rownames()`](https://rdrr.io/r/base/colnames.html) when present, or
`integer` row index values when there are no
[`rownames()`](https://rdrr.io/r/base/colnames.html). If heatmap
`row_labels` are defined, they are returned as
[`names()`](https://rdrr.io/r/base/names.html).

Note that [`names()`](https://rdrr.io/r/base/names.html) are assigned
inside [`try()`](https://rdrr.io/r/base/try.html) to allow the case
where `row_labels`, or `row_title` labels cannot be coerced to
`character` values, for example using `gridtext` for markdown
formatting.

Final note: It is best practice to draw the heatmap first with
[`ComplexHeatmap::draw()`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-dispatch.html)
then store the output in a new object. This step creates the definitive
clustering and therefore the row order is absolutely final, not subject
to potential randomness during clustering.

## See also

Other jam heatmap functions:
[`cell_fun_label()`](https://jmw86069.github.io/jamba/reference/cell_fun_label.md),
[`heatmap_column_order()`](https://jmw86069.github.io/jamba/reference/heatmap_column_order.md)

## Examples

``` r
# See heatmap_column_order() for examples
```
