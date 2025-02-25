
#' ComplexHeatmap cell function to label heatmap cells
#'
#' ComplexHeatmap cell function to label heatmap cells
#'
#' This function serves as a convenient method to add text
#' labels to each cell in a heatmap produced by
#' `ComplexHeatmap::Heatmap()`, via the argument `cell_fun`.
#'
#' Note that this function requires re-using the specific color
#' function used for the heatmap in the call to
#' `ComplexHeatmap::Heatmap()`.
#'
#' This function is slightly unique in that it allows multiple
#' labels, if `m` is supplied as a `list` of `matrix` objects.
#' In fact, some `matrix` objects may contain `character`
#' values with custom labels.
#'
#' Cell labels are colored based upon the heatmap cell color,
#' which is passed to `jamba::setTextContrastColor()` to determine
#' whether to use light or dark text color for optimum contrast.
#'
#' TODO: Option to supply a `logical` matrix to define a subset of
#' cells to label, for example only labels that meet a filter
#' criteria. Alternatively, the matrix data supplied in `m` can
#' already be filtered.
#'
#' TODO: Allow some matrix values that contain `character` data
#' to use `gridtext` for custom markdown formatting. That process
#' requires a slightly different method.
#'
#' @family jam heatmap functions
#'
#' @returns `function` sufficient to use as input to
#'    `ComplexHeatmap::Heatmap()` argument `cell_fun`.
#'
#' @param m `numeric` matrix or `list` of `matrix` objects. The
#'    first `matrix` object must be `numeric` and compatible
#'    with the color function `col_hm`.
#' @param prefix,suffix `character` vectors that define a prefix and
#'    suffix for each value in `m` for each cell.
#' @param cex `numeric` adjustment for the fontsize used for each label,
#'    which is multiplied by the default `fontsize=10` to determine
#'    the fontsize.
#' @param col_hm `function` as returned by `circlize::colorRamp2()` which
#'    should be the same function used to create the heatmap
#' @param outline `logical` indicating whether to draw an outline around
#'    each heatmap cell
#' @param abbrev `logical` indicating whether numeric values should
#'    be abbreviated using `jamba::asSize(..., kiloSize=1000)` which
#'    effectively reduces large numbers to `k` for thousands, `M` for
#'    millions (M for Mega), `G` for billions (G for Giga), etc.
#' @param show `integer` used when `m` is supplied as a `list` of matrices,
#'    in which case `show` is used to define which values should be used
#'    as cell labels. By default, all matrices are used.
#' @param rot `numeric` value used to rotate cell label text, default 0
#'    is horizontal.
#' @param sep `character` string, default `"\n"` newline, used when
#'    there are multiple labels per cell, which also requires
#'    `m` as a list, and `show` is `NULL` or has multiple values.
#' @param verbose `logical` indicating whether to print verbose output,
#'    specifically printing label information for position `(1, 1)`.
#'    This output will only be seen when rendering or building the
#'    Heatmap object.
#' @param ... additional arguments are ignored.
#'
#' @examplesIf (check_pkg_installed("ComplexHeatmap"))
#'
#' m <- matrix(stats::rnorm(16)*2, ncol=4)
#' colnames(m) <- LETTERS[1:4]
#' rownames(m) <- letters[1:4]
#' col_hm <- circlize::colorRamp2(breaks=(-2:2) * 2,
#'    colors=c("navy", "dodgerblue", "white", "tomato", "red4"))
#'
#' # the heatmap can be created in one step
#' hm <- ComplexHeatmap::Heatmap(m,
#'    col=col_hm,
#'    heatmap_legend_param=list(
#'       color_bar="discrete",
#'       border=TRUE,
#'       at=-4:4),
#'    cell_fun=cell_fun_label(m,
#'       col_hm=col_hm))
#' ComplexHeatmap::draw(hm)
#'
#' # the cell label function can be created first
#' cell_fun <- cell_fun_label(m,
#'    outline=TRUE,
#'    cex=1.5,
#'    col_hm=col_hm)
#' hm2 <- ComplexHeatmap::Heatmap(m,
#'    col=col_hm,
#'    cell_fun=cell_fun)
#' ComplexHeatmap::draw(hm2)
#'
#' @export
cell_fun_label <- function
(m,
 prefix="",
 suffix="",
 cex=1,
 col_hm=NULL,
 outline=FALSE,
 abbrev=FALSE,
 show=NULL,
 rot=0,
 sep="\n",
 verbose=FALSE,
 ...)
{
   if (!is.list(m)) {
      m <- list(m)
   }
   if (length(show) == 0) {
      show <- seq_along(m);
   }
   if (length(prefix) == 0) {
      prefix <- "";
   }
   if (length(suffix) == 0) {
      suffix <- "";
   }

   prefix <- rep(prefix, length.out=length(m));
   suffix <- rep(suffix, length.out=length(m));
   cell_fun_return <- function(j, i, x, y, width, height, fill) {
      cell_value <- rmNA(naValue=0, m[[1]][i, j]);
      if (length(col_hm) > 0) {
         cell_color <- col_hm(cell_value);
      } else {
         cell_color <- "white";
      }
      cell_label <- "";
      for (k in show) {
         mx <- m[[k]]
         cell_value2 <- rmNA(naValue=0, mx[i, j]);
         if (abbrev && max(cell_value2) >= 1000) {
            cell_label2 <- asSize(cell_value2,
               kiloSize=1000,
               unitType="",
               sep="");
         } else {
            cell_label2 <- format(cell_value2,
               big.mark=",",
               trim=TRUE)
         }
         cell_label <- paste(cell_label,
            paste0(prefix[k], cell_label2, suffix[k]),
            sep=sep);
         if (verbose && i == 1 && j == 1) {
            print("cell_label:");
            print(cell_label);
         }
      }
      # remove leading sep
      cell_label <- gsub(paste0("^", sep),
         "",
         cell_label);

      if (outline) {
         grid::grid.rect(x=x, y=y,
            width=width,
            height=height,
            gp=grid::gpar(
               col=jamba::setTextContrastColor(cell_color),
               fill=NA))
      }
      # blank cell_label
      # has no character length, or
      # is NA
      # contains no characters except " ", \t, \n, \r
      cell_label_isblank <- (nchar(cell_label) %in% 0 ||
            is.na(cell_label) ||
            !grepl("[^ \t\n\r]+", cell_label))
      if (!cell_label_isblank && cex > 0) {
         #fontsize <- (10 * cex) - floor(log10(cell_value)) * 1.3;
         fontsize <- (10 * cex);
         grid::grid.text(cell_label,
            x=x,
            y=y,
            rot=rot,
            gp=grid::gpar(
               fontsize=fontsize,
               fontface=1,
               col=jamba::setTextContrastColor(cell_color)
            ));
      }
   }
   cell_fun_return
}
