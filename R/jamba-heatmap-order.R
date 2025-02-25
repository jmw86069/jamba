
#' Return Heatmap row order from ComplexHeatmap heatmap object
#'
#' Return Heatmap row order from ComplexHeatmap heatmap object
#'
#' This function is a helpful utility to return the fully
#' qualified list of rownames in a `ComplexHeatmap::Heatmap`
#' object.
#'
#' The core intention is for the output to be usable with the
#' original data matrix used in the heatmap. Therefore, the
#' vector values are `rownames()` when present, or `integer`
#' row index values when there are no `rownames()`. If heatmap
#' `row_labels` are defined, they are returned as `names()`.
#'
#' Note that `names()` are assigned inside `try()` to allow the
#' case where `row_labels`, or `row_title` labels cannot be
#' coerced to `character` values, for example using `gridtext`
#' for markdown formatting.
#'
#' Final note: It is best practice to draw the heatmap first
#' with `ComplexHeatmap::draw()` then store the output in a new
#' object. This step creates the definitive clustering and
#' therefore the row order is absolutely final, not subject
#' to potential randomness during clustering.
#'
#' @returns output depends upon the heatmap:
#'    * When heatmap rows are grouped using `row_split`, and
#'    when the data matrix contains rownames,
#'    returns a `character` vector of rownames in the order they
#'    appear in the heatmap. When there are no rownames, `integer`
#'    row index values are returned. If the heatmap has row labels,
#'    they are returned as vector names.
#'    * When rows are grouped using `row_split`, it returns a
#'    `list` of vectors as described above. The `list`
#'    is named using the `row_title` labels only when there is
#'    an equal number of row labels.
#'
#' @family jam heatmap functions
#'
#' @param hm `Heatmap` or `HeatmapList` object as defined by the
#'    Bioconductor package via `ComplexHeatmap::Heatmap()`.
#' @param which_heatmap `integer`, default NULL, used when the input
#'    is a `HeatmapList` with multiple heatmaps.
#'
#' @examples
#' # See heatmap_column_order() for examples
#'
#' @export
heatmap_row_order <- function
(hm,
 which_heatmap=NULL)
{
   # TODO:
   # 1. Consider making this function work without calling ComplexHeatmap:
   #
   #   However, it will only work if the heatmap is already "prepared",
   #   which is defined by ComplexHeatmap::draw() or ComplexHeatmap::prepare().
   #
   #   For input "Heatmap" the telltale sign is hm@row_order_list is not empty.
   #   For input  "HeatmapList" the sign is hm@ht_list, then choose the first
   #   entry with class "Heatmap", then check for example if
   #   hm@ht_list[[1]]@row_order_list is empty.
   #

   # validate input class and ComplexHeatmap package
   if (!any(c("Heatmap", "HeatmapList") %in% class(hm))) {
      stop(paste(
         "Input hm must be class 'Heatmap' or 'HeatmapList',",
         "from the ComplexHeatmap Bioconductor package."));
   }
   if (!check_pkg_installed("ComplexHeatmap")) {
      stop(paste("This function requires the ComplexHeatmap Bioconductor package.",
         "Install with \"BiocManager::install('ComplexHeatmap')\".",
         "BiocManageer can be installed with with \"install.packages('BiocManager')\"."));
   }
   if ("HeatmapList" %in% class(hm)) {
      # convert logical to numeric vector
      if (is.logical(which_heatmap)) {
         which_heatmap <- which(which_heatmap);
      }
      # force character to match existing heatmap names
      if (is.character(which_heatmap)) {
         which_heatmap <- intersect(which_heatmap, names(hm@ht_list))
      }
      if (length(which_heatmap) == 0) {
         hm_sdim <- sdim(hm@ht_list);
         which_heatmap <- match("Heatmap", hm_sdim$class)
         hm <- hm@ht_list[[which_heatmap]];
      } else if (length(which_heatmap) == 1) {
         hm <- hm@ht_list[[which_heatmap]];
      } else {
         if (is.numeric(which_heatmap)) {
            hm_sdim <- subset(sdim(hm@ht_list), !grepl("Annotation", class));
            hm_names <- rmNA(nameVector(rownames(hm_sdim)[which_heatmap]))
         } else {
            # hm_names <- nameVectorN(hm@ht_list);
            hm_names <- nameVector(which_heatmap)
         }
         hm_orders <- lapply(hm_names, function(hm_name){
            heatmap_row_order(hm=hm,
               which_heatmap=hm_name)
         })
         return(hm_orders)
      }
   }

   x_row_order <- ComplexHeatmap::row_order(hm);
   x <- lapply(x_row_order, function(i){
      if (length(rownames(hm@matrix)) > 0) {
         j <- rownames(hm@matrix)[i];
      } else {
         j <- i;
      }
      if (length(hm@row_names_param$labels) > 0) {
         try(silent=TRUE,
            names(j) <- hm@row_names_param$labels[i]);
      }
      j
   });

   if (is.atomic(x_row_order) && is.list(x)) {
      x <- unlist(unname(x));
   } else {
      if (length(hm@row_title) == length(x)) {
         names(x) <- hm@row_title;
      }
   }
   x;
}

#' Return Heatmap column order from ComplexHeatmap heatmap object
#'
#' Return Heatmap column order from ComplexHeatmap heatmap object
#'
#' This function is a helpful utility to return the fully
#' qualified list of colnames in a `ComplexHeatmap::Heatmap`
#' object.
#'
#' The core intention is for the output to be usable with the
#' original data matrix used in the heatmap. Therefore, the
#' vector values are `colnames()` when present, or `integer`
#' column index values when there are no `colnames()`. If heatmap
#' `column_labels` are defined, they are returned as `names()`.
#'
#' Note that `names()` are assigned inside `try()` to allow the
#' case where `column_labels`, or `column_title` labels cannot be
#' coerced to `character` values, for example using `gridtext`
#' for markdown formatting.
#'
#' @returns output depends upon the heatmap:
#'    * When heatmap columns are grouped using `column_split`, and
#'    when the data matrix contains colnames,
#'    returns a `character` vector of colnames in the order they
#'    appear in the heatmap. When there are no colnames, `integer`
#'    column index values are returned. If the heatmap has column labels,
#'    they are returned as vector names.
#'    * When columns are grouped using `column_split`, it returns a
#'    `list` of vectors as described above. The `list`
#'    is named using the `column_title` labels only when there is
#'    an equal number of column labels.
#'
#' @family jam heatmap functions
#'
#' @param hm `Heatmap` or `HeatmapList` object as defined by the
#'    Bioconductor package via `ComplexHeatmap::Heatmap()`.
#' @param which_heatmap used to specify a specific heatmap with `hm` is
#'    provided as a `HeatmapList`. When `NULL` (default) the first heatmap
#'    in `hm@ht_list` is used. When one value is supplied, only that heatmap
#'    is used. When multiple values are supplied, a `list` is returned.
#'    Input can be either:
#'    * `numeric` - indicating the heatmap number in `hm@ht_list`
#'    * `character` - indicating the heatmap name seen in `names(hm@ht_list)`
#'
#'
#' @examples
#' if (check_pkg_installed("ComplexHeatmap")) {
#'    set.seed(123);
#'
#'    mat <- matrix(stats::rnorm(18 * 24),
#'       ncol=24);
#'    rownames(mat) <- paste0("row", seq_len(18))
#'    colnames(mat) <- paste0("column", seq_len(24))
#'
#'    # obtaining row order first causes a warning message
#'    hm1 <- ComplexHeatmap::Heatmap(mat);
#'
#'    # best practice is to draw() and store output in an object
#'    # to ensure the row orders are absolutely fixed
#'    hm1_drawn <- ComplexHeatmap::draw(hm1);
#'    print(heatmap_row_order(hm1_drawn))
#'    print(heatmap_column_order(hm1_drawn))
#'
#'    # row and column split
#'    hm1_split <- ComplexHeatmap::Heatmap(mat,
#'       column_split=3, row_split=3, border=TRUE);
#'    hm1_split_drawn <- ComplexHeatmap::draw(hm1_split);
#'    print(heatmap_row_order(hm1_split_drawn))
#'    print(heatmap_column_order(hm1_split_drawn))
#'
#'    # display two heatmaps side-by-side
#'    mat2 <- mat + stats::rnorm(18*24);
#'    hm2 <- ComplexHeatmap::Heatmap(mat2, border=TRUE, row_split=4);
#'
#'    hm1hm2_drawn <- ComplexHeatmap::draw(hm1_split + hm2,
#'       ht_gap=grid::unit(1, "cm"));
#'    print(heatmap_row_order(hm1hm2_drawn))
#'    print(heatmap_row_order(hm1hm2_drawn, which_heatmap=2))
#'    # by default the order uses the first heatmap
#'    print(heatmap_column_order(hm1hm2_drawn))
#'    # the second heatmap can be returned
#'    print(heatmap_column_order(hm1hm2_drawn, which_heatmap=2))
#'    # or a list of heatmap orders can be returned
#'    print(heatmap_column_order(hm1hm2_drawn, which_heatmap=1:2))
#'
#'    # stacked vertical heatmaps
#'    hm1hm2_drawn_tall <- ComplexHeatmap::draw(
#'       ComplexHeatmap::`%v%`(hm1_split, hm2),
#'       ht_gap=grid::unit(1, "cm"));
#'    print(heatmap_row_order(hm1hm2_drawn))
#'    print(heatmap_row_order(hm1hm2_drawn, which_heatmap=2))
#'    print(heatmap_row_order(hm1hm2_drawn, which_heatmap=1:2))
#'    print(heatmap_row_order(hm1hm2_drawn,
#'       which_heatmap=names(hm1hm2_drawn@ht_list)))
#'
#'    # annotation heatmap
#'    ha <- ComplexHeatmap::rowAnnotation(left=rownames(mat))
#'    ha_drawn <- ComplexHeatmap::draw(ha + hm1)
#'    print(sdim(ha_drawn@ht_list))
#'    print(heatmap_row_order(ha_drawn))
#'    print(heatmap_column_order(ha_drawn))
#'
#'    # stacked vertical heatmaps with top annotation
#'    ta <- ComplexHeatmap::HeatmapAnnotation(top=colnames(mat))
#'    hm1_ha <- ComplexHeatmap::Heatmap(mat,
#'       left_annotation=ha,
#'       column_split=3, row_split=3, border=TRUE);
#'    hm1hm2_drawn_tall <- ComplexHeatmap::draw(
#'       ComplexHeatmap::`%v%`(ta,
#'          ComplexHeatmap::`%v%`(hm1_ha, hm2)),
#'       ht_gap=grid::unit(1, "cm"));
#'    print(sdim(hm1hm2_drawn_tall@ht_list))
#'    print(heatmap_row_order(hm1hm2_drawn_tall))
#'    print(heatmap_row_order(hm1hm2_drawn_tall, 2))
#' }
#'
#' @export
heatmap_column_order <- function
(hm,
 which_heatmap=NULL)
{
   ##
   # validate input class and ComplexHeatmap package
   if (!any(c("Heatmap", "HeatmapList") %in% class(hm))) {
      stop(paste(
         "Input hm must be class 'Heatmap' or 'HeatmapList',",
         "from the ComplexHeatmap Bioconductor package."));
   }
   if (!check_pkg_installed("ComplexHeatmap")) {
      stop(paste(
         "This function requires the ComplexHeatmap Bioconductor package.",
         "Install with \"BiocManager::install('ComplexHeatmap')\".",
         "BiocManager can be installed with",
         "\"install.packages('BiocManager')\"."));
   }
   if ("HeatmapList" %in% class(hm)) {
      # convert logical to numeric vector
      if (is.logical(which_heatmap)) {
         which_heatmap <- which(which_heatmap);
      }
      # force character to match existing heatmap names
      if (is.character(which_heatmap)) {
         which_heatmap <- intersect(which_heatmap, names(hm@ht_list))
      }
      if (length(which_heatmap) == 0) {
         hm_sdim <- sdim(hm@ht_list);
         which_heatmap <- match("Heatmap", hm_sdim$class)
         hm <- hm@ht_list[[which_heatmap]];
      } else if (length(which_heatmap) == 1) {
         hm <- hm@ht_list[[which_heatmap]];
      } else {
         if (is.numeric(which_heatmap)) {
            hm_sdim <- subset(sdim(hm@ht_list), !grepl("Annotation", class));
            hm_names <- rmNA(nameVector(rownames(hm_sdim)[which_heatmap]))
         } else {
            # hm_names <- nameVectorN(hm@ht_list);
            hm_names <- nameVector(which_heatmap)
         }
         hm_orders <- lapply(hm_names, function(hm_name){
            heatmap_column_order(hm=hm,
               which_heatmap=hm_name)
         })
         return(hm_orders)
      }
   }
   x_column_order <- ComplexHeatmap::column_order(hm);
   x <- lapply(x_column_order, function(i){
      if (length(colnames(hm@matrix)) > 0) {
         j <- colnames(hm@matrix)[i];
      } else {
         j <- i;
      }
      if (length(hm@column_names_param$labels) > 0) {
         try(silent=TRUE,
            names(j) <- hm@column_names_param$labels[i]);
      }
      j
   });

   if (is.atomic(x_column_order) && is.list(x)) {
      x <- unlist(x);
   } else {
      if (length(hm@column_title) == length(x)) {
         try(silent=TRUE,
            names(x) <- hm@column_title);
      }
   }
   x;
}
