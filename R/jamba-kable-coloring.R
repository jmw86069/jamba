
#' Extend kableExtra colorization of Rmarkdown tables
#'
#' Extend kableExtra colorization of Rmarkdown tables
#'
#' This function extends the `kableExtra` package, and is only
#' available for use if the `kableExtra` package is installed. It is
#' intended to allow specific color assignment of elements in a
#' data.frame, but otherwise uses the `kableExtra` functions to
#' apply those colors.
#'
#' @param df data.frame
#' @param colorSub named vector of R colors, whose names match entries
#'    in the `data.frame` which are given these assigned colors.
#' @param background_as_tile boolean defining whether a cell background
#'    color will appear as a rounded tile if `TRUE`, or a rectangle
#'    if `FALSE`.
#' @param color_cells boolean indicating whether to color individual cells
#' @param row_color_by optional vector of `colnames` found in `df`
#'    indicating how to colorize entire rows of a table. The typical
#'    colorization by cell will not fill the whole cell per row, so
#'    this colorization can be helpful to fill the remaining area.
#' @param sep character delimiter used to combine values in multiple
#'    columns when `row_color_by` is supplied and contains multiple
#'    `colnames`. The delimited character strings are compared to
#'    `colorSub` to assign colors.
#' @param return_type character string indicating the type of data
#'    to return. If `row_color_by` is supplied, then the only
#'    `"kable"` is allowed, otherwise `"data.frame"` is returned.
#'    Note that the `"kable"` option runs \code{\%>\% row_spec()} which
#'    returns an object only usable in downstream `kable` calls.
#' @param verbose boolean indicating whether to print verbose output.
#'
#' @return data.frame or kable data.frame dependent upon the
#'    `return_type` argument. Note that even the `data.frame` returned
#'    will have colors encoded into each cell, so it will likely
#'    be difficult to manipulate.
#'
#' @examples
#' # Note the output will produce HTML code, which will
#' # open the Rstudio viewer when run inside Rstudio.
#' #kable_coloring(
#' #   df=data.frame(A=letters[1:5], B=LETTERS[1:5]),
#' #   colorSub=nameVector(rainbow(10), c(letters[1:5], LETTERS[1:5])),
#' #   row_color_by="B")
#'
#' @family jam color functions
#' @family jam practical functions
#'
#' @export
kable_coloring <- function
(df,
 colorSub=NULL,
 background_as_tile=TRUE,
 color_cells=TRUE,
 row_color_by=NULL,
 sep="_",
 return_type=c("kable",
    "data.frame"),
 verbose=FALSE,
 ...)
{
   ## Purpose is to enhance kableExtra by automating some manual steps
   ## required to colorize text fields in a data.frame
   ##
   ## The input is expected to be a data.frame
   if (!suppressPackageStartupMessages(require(kableExtra))) {
      stop("kable_coloring requires the kableExtra package");
   }
   if (!suppressPackageStartupMessages(require(dplyr))) {
      stop("kable_coloring requires the dplyr package");
   }
   return_type <- match.arg(return_type);

   if (return_type %in% "kable" || length(row_color_by) > 0) {
      df1 <- df;
   }

   if (color_cells) {
      for (i in colnames(df)) {
         if (any(as.character(df[[i]]) %in% names(colorSub))) {
            if (verbose) {
               printDebug("kable_coloring(): ",
                  "colorizing column:",
                  i);
            }
            df[[i]] <- cell_spec(df[[i]],
               background_as_tile=background_as_tile,
               color=setTextContrastColor(
                  rmNA(colorSub[as.character(df[[i]])],
                     naValue="transparent")
               ),
               background=rmNA(colorSub[as.character(df[[i]])],
                  naValue="transparent"))
         } else {
            if (verbose) {
               printDebug("kable_coloring(): ",
                  "No values to colorize in column:",
                  i,
                  fgText=c("orange","red"));
            }
         }
      }
   }

   ## Convert to kable
   if (return_type %in% "kable" || length(row_color_by) > 0) {
      df <- df %>%
         kable(escape=FALSE,
            ...) %>%
         kable_styling();
   }

   ## Optionally color rows using values in a column
   if (length(row_color_by) > 0) {
      if (verbose) {
         printDebug("kable_coloring(): ",
            "row_color_by:",
            row_color_by);
      }
      row_color_by_value <- as.character(
         pasteByRow(df1[,row_color_by,drop=FALSE],
            sep=sep));
      row_colors <- colorSub[row_color_by_value];
      if (verbose) {
         printDebug("kable_coloring(): ",
            "row_color_by_value:",
            row_color_by_value);
         printDebug("kable_coloring(): ",
            "row_colors:",
            row_colors);
      }
      row_colors_valid <- unique(rmNA(row_colors));
      for (row_color in row_colors_valid) {
         which_rows <- which(row_colors %in% row_color);
         if (verbose) {
            printDebug("kable_coloring(): ",
               "colorizing rows:",
               which_rows,
               closestRcolor(row_color),
               fgText=c("orange","lightblue",
                  "orange",
                  row_color))
         }
         df <- df %>%
            row_spec(which_rows,
               background=row_color,
               color=setTextContrastColor(row_color));
      }
   }

   df;
}
