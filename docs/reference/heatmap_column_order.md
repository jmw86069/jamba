# Return Heatmap column order from ComplexHeatmap heatmap object

Return Heatmap column order from ComplexHeatmap heatmap object

## Usage

``` r
heatmap_column_order(hm, which_heatmap = NULL)
```

## Arguments

- hm:

  `Heatmap` or `HeatmapList` object as defined by the Bioconductor
  package via
  [`ComplexHeatmap::Heatmap()`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html).

- which_heatmap:

  used to specify a specific heatmap with `hm` is provided as a
  `HeatmapList`. When 'NULL' (default) the first heatmap in `hm@ht_list`
  is used. When one value is supplied, only that heatmap is used. When
  multiple values are supplied, a `list` is returned. Input can be
  either:

  - `numeric` - indicating the heatmap number in `hm@ht_list`

  - `character` - indicating the heatmap name seen in
    `names(hm@ht_list)`

## Value

output depends upon the heatmap:

- When heatmap columns are grouped using `column_split`, and when the
  data matrix contains colnames, returns a `character` vector of
  colnames in the order they appear in the heatmap. When there are no
  colnames, `integer` column index values are returned. If the heatmap
  has column labels, they are returned as vector names.

- When columns are grouped using `column_split`, it returns a `list` of
  vectors as described above. The `list` is named using the
  `column_title` labels only when there is an equal number of column
  labels.

## Details

This function is a helpful utility to return the fully qualified list of
colnames in a
[`ComplexHeatmap::Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html)
object.

The core intention is for the output to be usable with the original data
matrix used in the heatmap. Therefore, the vector values are
[`colnames()`](https://rdrr.io/r/base/colnames.html) when present, or
`integer` column index values when there are no
[`colnames()`](https://rdrr.io/r/base/colnames.html). If heatmap
`column_labels` are defined, they are returned as
[`names()`](https://rdrr.io/r/base/names.html).

Note that [`names()`](https://rdrr.io/r/base/names.html) are assigned
inside [`try()`](https://rdrr.io/r/base/try.html) to allow the case
where `column_labels`, or `column_title` labels cannot be coerced to
`character` values, for example using `gridtext` for markdown
formatting.

## See also

Other jam heatmap functions:
[`cell_fun_label()`](https://jmw86069.github.io/jamba/reference/cell_fun_label.md),
[`heatmap_row_order()`](https://jmw86069.github.io/jamba/reference/heatmap_row_order.md)

## Examples

``` r
if (check_pkg_installed("ComplexHeatmap")) {
   set.seed(123);

   mat <- matrix(stats::rnorm(18 * 24),
      ncol=24);
   rownames(mat) <- paste0("row", seq_len(18))
   colnames(mat) <- paste0("column", seq_len(24))

   # obtaining row order first causes a warning message
   hm1 <- ComplexHeatmap::Heatmap(mat);

   # best practice is to draw() and store output in an object
   # to ensure the row orders are absolutely fixed
   hm1_drawn <- ComplexHeatmap::draw(hm1);
   print(heatmap_row_order(hm1_drawn))
   print(heatmap_column_order(hm1_drawn))

   # row and column split
   hm1_split <- ComplexHeatmap::Heatmap(mat,
      column_split=3, row_split=3, border=TRUE);
   hm1_split_drawn <- ComplexHeatmap::draw(hm1_split);
   print(heatmap_row_order(hm1_split_drawn))
   print(heatmap_column_order(hm1_split_drawn))

   # display two heatmaps side-by-side
   mat2 <- mat + stats::rnorm(18*24);
   hm2 <- ComplexHeatmap::Heatmap(mat2, border=TRUE, row_split=4);

   hm1hm2_drawn <- ComplexHeatmap::draw(hm1_split + hm2,
      ht_gap=grid::unit(1, "cm"));
   print(heatmap_row_order(hm1hm2_drawn))
   print(heatmap_row_order(hm1hm2_drawn, which_heatmap=2))
   # by default the order uses the first heatmap
   print(heatmap_column_order(hm1hm2_drawn))
   # the second heatmap can be returned
   print(heatmap_column_order(hm1hm2_drawn, which_heatmap=2))
   # or a list of heatmap orders can be returned
   print(heatmap_column_order(hm1hm2_drawn, which_heatmap=1:2))

   # stacked vertical heatmaps
   hm1hm2_drawn_tall <- ComplexHeatmap::draw(
      ComplexHeatmap::`%v%`(hm1_split, hm2),
      ht_gap=grid::unit(1, "cm"));
   print(heatmap_row_order(hm1hm2_drawn))
   print(heatmap_row_order(hm1hm2_drawn, which_heatmap=2))
   print(heatmap_row_order(hm1hm2_drawn, which_heatmap=1:2))
   print(heatmap_row_order(hm1hm2_drawn,
      which_heatmap=names(hm1hm2_drawn@ht_list)))

   # annotation heatmap
   ha <- ComplexHeatmap::rowAnnotation(left=rownames(mat))
   ha_drawn <- ComplexHeatmap::draw(ha + hm1)
   print(sdim(ha_drawn@ht_list))
   print(heatmap_row_order(ha_drawn))
   print(heatmap_column_order(ha_drawn))

   # stacked vertical heatmaps with top annotation
   ta <- ComplexHeatmap::HeatmapAnnotation(top=colnames(mat))
   hm1_ha <- ComplexHeatmap::Heatmap(mat,
      left_annotation=ha,
      column_split=3, row_split=3, border=TRUE);
   hm1hm2_drawn_tall <- ComplexHeatmap::draw(
      ComplexHeatmap::`%v%`(ta,
         ComplexHeatmap::`%v%`(hm1_ha, hm2)),
      ht_gap=grid::unit(1, "cm"));
   print(sdim(hm1hm2_drawn_tall@ht_list))
   print(heatmap_row_order(hm1hm2_drawn_tall))
   print(heatmap_row_order(hm1hm2_drawn_tall, 2))
}

#>   row16    row2   row12    row7   row17    row9    row8    row1   row18    row5 
#> "row16"  "row2" "row12"  "row7" "row17"  "row9"  "row8"  "row1" "row18"  "row5" 
#>    row4    row3    row6   row11   row14   row15   row13   row10 
#>  "row4"  "row3"  "row6" "row11" "row14" "row15" "row13" "row10" 
#>   column15    column1   column17    column6    column9   column23   column14 
#> "column15"  "column1" "column17"  "column6"  "column9" "column23" "column14" 
#>   column10    column4   column11    column7   column18   column12   column21 
#> "column10"  "column4" "column11"  "column7" "column18" "column12" "column21" 
#>   column19   column24   column13    column8   column16    column5    column2 
#> "column19" "column24" "column13"  "column8" "column16"  "column5"  "column2" 
#>    column3   column22   column20 
#>  "column3" "column22" "column20" 

#> $`1`
#>    row2   row16   row12    row1    row8    row9    row7   row17 
#>  "row2" "row16" "row12"  "row1"  "row8"  "row9"  "row7" "row17" 
#> 
#> $`2`
#>   row18 
#> "row18" 
#> 
#> $`3`
#>   row10   row13   row15    row4    row5    row3    row6   row11   row14 
#> "row10" "row13" "row15"  "row4"  "row5"  "row3"  "row6" "row11" "row14" 
#> 
#> $`1`
#>   column10    column4    column7   column11    column1   column15    column6 
#> "column10"  "column4"  "column7" "column11"  "column1" "column15"  "column6" 
#>   column17    column9   column14   column23 
#> "column17"  "column9" "column14" "column23" 
#> 
#> $`2`
#>    column2    column5   column16   column20    column3   column22 
#>  "column2"  "column5" "column16" "column20"  "column3" "column22" 
#> 
#> $`3`
#>    column8   column13   column24   column19   column21   column12   column18 
#>  "column8" "column13" "column24" "column19" "column21" "column12" "column18" 
#> 

#> $`1`
#>    row2   row16   row12    row1    row8    row9    row7   row17 
#>  "row2" "row16" "row12"  "row1"  "row8"  "row9"  "row7" "row17" 
#> 
#> $`2`
#>   row18 
#> "row18" 
#> 
#> $`3`
#>   row10   row13   row15    row4    row5    row3    row6   row11   row14 
#> "row10" "row13" "row15"  "row4"  "row5"  "row3"  "row6" "row11" "row14" 
#> 
#> [[1]]
#>    row2   row16   row12    row1    row8    row9    row7   row17 
#>  "row2" "row16" "row12"  "row1"  "row8"  "row9"  "row7" "row17" 
#> 
#> [[2]]
#>   row18 
#> "row18" 
#> 
#> [[3]]
#>   row10   row13   row15    row4    row5    row3    row6   row11   row14 
#> "row10" "row13" "row15"  "row4"  "row5"  "row3"  "row6" "row11" "row14" 
#> 
#> $`1`
#>   column10    column4    column7   column11    column1   column15    column6 
#> "column10"  "column4"  "column7" "column11"  "column1" "column15"  "column6" 
#>   column17    column9   column14   column23 
#> "column17"  "column9" "column14" "column23" 
#> 
#> $`2`
#>    column2    column5   column16   column20    column3   column22 
#>  "column2"  "column5" "column16" "column20"  "column3" "column22" 
#> 
#> $`3`
#>    column8   column13   column24   column19   column21   column12   column18 
#>  "column8" "column13" "column24" "column19" "column21" "column12" "column18" 
#> 
#>   column18   column21   column19    column2   column16    column5   column24 
#> "column18" "column21" "column19"  "column2" "column16"  "column5" "column24" 
#>   column13    column3    column7   column11   column20   column15   column17 
#> "column13"  "column3"  "column7" "column11" "column20" "column15" "column17" 
#>   column23   column12   column14   column22    column1    column6    column4 
#> "column23" "column12" "column14" "column22"  "column1"  "column6"  "column4" 
#>   column10    column8    column9 
#> "column10"  "column8"  "column9" 
#> $matrix_4
#> $matrix_4$`1`
#>   column10    column4    column7   column11    column1   column15    column6 
#> "column10"  "column4"  "column7" "column11"  "column1" "column15"  "column6" 
#>   column17    column9   column14   column23 
#> "column17"  "column9" "column14" "column23" 
#> 
#> $matrix_4$`2`
#>    column2    column5   column16   column20    column3   column22 
#>  "column2"  "column5" "column16" "column20"  "column3" "column22" 
#> 
#> $matrix_4$`3`
#>    column8   column13   column24   column19   column21   column12   column18 
#>  "column8" "column13" "column24" "column19" "column21" "column12" "column18" 
#> 
#> 
#> $matrix_5
#>   column18   column21   column19    column2   column16    column5   column24 
#> "column18" "column21" "column19"  "column2" "column16"  "column5" "column24" 
#>   column13    column3    column7   column11   column20   column15   column17 
#> "column13"  "column3"  "column7" "column11" "column20" "column15" "column17" 
#>   column23   column12   column14   column22    column1    column6    column4 
#> "column23" "column12" "column14" "column22"  "column1"  "column6"  "column4" 
#>   column10    column8    column9 
#> "column10"  "column8"  "column9" 
#> 

#> $`1`
#>    row2   row16   row12    row1    row8    row9    row7   row17 
#>  "row2" "row16" "row12"  "row1"  "row8"  "row9"  "row7" "row17" 
#> 
#> $`2`
#>   row18 
#> "row18" 
#> 
#> $`3`
#>   row10   row13   row15    row4    row5    row3    row6   row11   row14 
#> "row10" "row13" "row15"  "row4"  "row5"  "row3"  "row6" "row11" "row14" 
#> 
#> [[1]]
#>    row2   row16   row12    row1    row8    row9    row7   row17 
#>  "row2" "row16" "row12"  "row1"  "row8"  "row9"  "row7" "row17" 
#> 
#> [[2]]
#>   row18 
#> "row18" 
#> 
#> [[3]]
#>   row10   row13   row15    row4    row5    row3    row6   row11   row14 
#> "row10" "row13" "row15"  "row4"  "row5"  "row3"  "row6" "row11" "row14" 
#> 
#> $matrix_4
#> $matrix_4$`1`
#>    row2   row16   row12    row1    row8    row9    row7   row17 
#>  "row2" "row16" "row12"  "row1"  "row8"  "row9"  "row7" "row17" 
#> 
#> $matrix_4$`2`
#>   row18 
#> "row18" 
#> 
#> $matrix_4$`3`
#>   row10   row13   row15    row4    row5    row3    row6   row11   row14 
#> "row10" "row13" "row15"  "row4"  "row5"  "row3"  "row6" "row11" "row14" 
#> 
#> 
#> $matrix_5
#> $matrix_5[[1]]
#>    row2   row16   row12    row1    row8    row9    row7   row17 
#>  "row2" "row16" "row12"  "row1"  "row8"  "row9"  "row7" "row17" 
#> 
#> $matrix_5[[2]]
#>   row18 
#> "row18" 
#> 
#> $matrix_5[[3]]
#>   row10   row13   row15    row4    row5    row3    row6   row11   row14 
#> "row10" "row13" "row15"  "row4"  "row5"  "row3"  "row6" "row11" "row14" 
#> 
#> 
#> $matrix_4
#> $matrix_4$`1`
#>    row2   row16   row12    row1    row8    row9    row7   row17 
#>  "row2" "row16" "row12"  "row1"  "row8"  "row9"  "row7" "row17" 
#> 
#> $matrix_4$`2`
#>   row18 
#> "row18" 
#> 
#> $matrix_4$`3`
#>   row10   row13   row15    row4    row5    row3    row6   row11   row14 
#> "row10" "row13" "row15"  "row4"  "row5"  "row3"  "row6" "row11" "row14" 
#> 
#> 
#> $matrix_5
#> $matrix_5[[1]]
#>    row2   row16   row12    row1    row8    row9    row7   row17 
#>  "row2" "row16" "row12"  "row1"  "row8"  "row9"  "row7" "row17" 
#> 
#> $matrix_5[[2]]
#>   row18 
#> "row18" 
#> 
#> $matrix_5[[3]]
#>   row10   row13   row15    row4    row5    row3    row6   row11   row14 
#> "row10" "row13" "row15"  "row4"  "row5"  "row3"  "row6" "row11" "row14" 
#> 
#> 

#>                      rows cols             class
#> heatmap_annotation_0    1      HeatmapAnnotation
#> matrix_3               18   24           Heatmap
#>   row16    row2   row12    row7   row17    row9    row8    row1   row18    row5 
#> "row16"  "row2" "row12"  "row7" "row17"  "row9"  "row8"  "row1" "row18"  "row5" 
#>    row4    row3    row6   row11   row14   row15   row13   row10 
#>  "row4"  "row3"  "row6" "row11" "row14" "row15" "row13" "row10" 
#>   column15    column1   column17    column6    column9   column23   column14 
#> "column15"  "column1" "column17"  "column6"  "column9" "column23" "column14" 
#>   column10    column4   column11    column7   column18   column12   column21 
#> "column10"  "column4" "column11"  "column7" "column18" "column12" "column21" 
#>   column19   column24   column13    column8   column16    column5    column2 
#> "column19" "column24" "column13"  "column8" "column16"  "column5"  "column2" 
#>    column3   column22   column20 
#>  "column3" "column22" "column20" 

#>                      rows cols             class
#> heatmap_annotation_1    1      HeatmapAnnotation
#> matrix_6               18   24           Heatmap
#> matrix_5               18   24           Heatmap
#> $`1`
#>    row2   row16   row12    row1    row8    row9    row7   row17 
#>  "row2" "row16" "row12"  "row1"  "row8"  "row9"  "row7" "row17" 
#> 
#> $`2`
#>   row18 
#> "row18" 
#> 
#> $`3`
#>   row10   row13   row15    row4    row5    row3    row6   row11   row14 
#> "row10" "row13" "row15"  "row4"  "row5"  "row3"  "row6" "row11" "row14" 
#> 
#> $`1`
#>    row2   row16   row12    row1    row8    row9    row7   row17 
#>  "row2" "row16" "row12"  "row1"  "row8"  "row9"  "row7" "row17" 
#> 
#> $`2`
#>   row18 
#> "row18" 
#> 
#> $`3`
#>   row10   row13   row15    row4    row5    row3    row6   row11   row14 
#> "row10" "row13" "row15"  "row4"  "row5"  "row3"  "row6" "row11" "row14" 
#> 
```
