

#' Show colors from a vector or list
#'
#' Show colors from a vector or list
#'
#' This function simply displays colors for review, using
#' `imageByColors()` to display colors and labels across the
#' plot space.
#'
#' When supplied a `list`, each row in `imageByColors()` represents
#' an entry in the `list`. Nothing fancy.
#'
#' @family jam plot functions
#' @family jam color functions
#'
#' @param x one of these input types:
#'    * `character` vector of colors
#'    * `function` to produce colors, for example `circlize::colorRamp2()`
#'    * `list` with any combination of `character` or `function`
#' @param labelCells `logical` whether to label colors atop the color itself.
#'    If NULL (default) it will only display labels with 40 or fewer items
#'    on either axis.
#' @param transpose `logical` whether to transpose the colors to display
#'    top-to-bottom, instead of left-to-right.
#' @param srtCellnote `numeric` angle to rotate text when
#'    \code{labelCells=TRUE}. When set to NULL, labels are vertical
#'    srtCellnote=90 when \code{transpose=FALSE} and horizontal
#'    srtCellnote=0 when \code{transpose=TRUE}.
#' @param adjustMargins `logical` indicating whether to call
#'    `adjustAxisLabelMargins()` to adjust the x- and y-axis
#'    label margins to accomodate the label size.
#'    * Note when an axis is hidden by using `xaxt="n"` or `xaxt="n"`,
#'    the respective margin will not be adjusted.
#'    * The arguments in `...` take precedence over `graphics::par()`,
#'    when deciding whether to adjust margins. However if `xaxt="s"` and
#'    `graphics::par("xaxt"="n")` the margin will be adjusted but not
#'    displayed.
#'    In this way the axes can be adjusted without displaying the labels,
#'    so the labels can be rendered later if needed.
#' @param makeUnique `logical` indicating whether to display only the first
#'    unique color. When `x` is supplied as a `list` this operation will
#'    display the first unique color for each `list` element.
#'    Also, when `x` is a `list`, just to be fancy, `makeUnique`
#'    is recycled to `length(x)` so certain list elements can display
#'    unique values, while others display all values.
#' @param doPlot `logical` indicating whether to produce a visual plot.
#'    Note this function returns the color matrix invisibly.
#' @param ... additional parameters are passed to `imageByColors()`.
#'
#' @returns invisible color `matrix` used by `imageByColors()`. When
#'    the input `x` is empty, or cannot be converted to colors when
#'    `x` contains a `function`, the output returns `NULL`.
#'
#' @examples
#' x <- color2gradient(list(Reds=c("red"), Blues=c("blue")), n=c(4,7));
#' showColors(x);
#'
#' showColors(getColorRamp("firebrick3"))
#'
#' if (requireNamespace("RColorBrewer", quietly=TRUE)) {
#'    RColorBrewer_namelist <- rownames(RColorBrewer::brewer.pal.info);
#'    y <- lapply(nameVector(RColorBrewer_namelist), function(i){
#'       n <- RColorBrewer::brewer.pal.info[i, "maxcolors"]
#'       j <- RColorBrewer::brewer.pal(n, i);
#'       nameVector(j, seq_along(j));
#'    });
#'    showColors(y, cexCellnote=0.6, cex.axis=0.7, main="Brewer Colors");
#' }
#' if (requireNamespace("viridisLite", quietly=TRUE)) {
#'    # given one function name it will display discrete colors
#'    showColors(viridisLite::viridis)
#'    # a list of functions will show each function output
#'    showColors(list(viridis=viridisLite::viridis,
#'       inferno=viridisLite::inferno))
#'
#'    # grab the full viridis color map
#'    z <- rgb2col(viridisLite::viridis.map[,c("R","G","B")]);
#'    # split the colors into a list
#'    viridis_names <- c(A="magma",
#'       B="inferno",
#'       C="plasma",
#'       D="viridis",
#'       E="cividis",
#'       F="rocket",
#'       G="mako",
#'       H="turbo")
#'    y <- split(z,
#'       paste0(viridisLite::viridis.map$opt, ": ",
#'       viridis_names[viridisLite::viridis.map$opt]));
#'    showColors(y, labelCells=TRUE, xaxt="n", main="viridis.map colors");
#' }
#'
#' # demonstrate makeUnique=TRUE
#' j1 <- getColorRamp("rainbow", n=7);
#' names(j1) <- seq_along(j1);
#' j2 <- rep(j1, each=3);
#' names(j2) <- makeNames(names(j2), suffix="_rep");
#' j2
#' showColors(list(
#'    j1=j1,
#'    j2=j2,
#'    j3=j2),
#'    makeUnique=c(FALSE, FALSE, TRUE))
#'
#' @export
showColors <- function
(x,
 labelCells=NULL,
 transpose=FALSE,
 srtCellnote=NULL,
 adjustMargins=TRUE,
 makeUnique=FALSE,
 doPlot=TRUE,
 ...)
{
   ## Purpose is to show a vector of colors, or display a list of
   ## color vectors in a table view
   ## x <- color2gradient(list(Reds=c("red"), Blues=c("blue")), n=c(4,7))
   if (length(x) == 0) {
      return(invisible(x));
   }

   # light wrapper to convert a function to vector of colors
   fn_to_color <- function(f, n=7, ...) {
      if (is.function(f)) {
         # circlize::colorRamp2() output
         if (all(c("colors", "breaks") %in% names(attributes(f)))) {
            colorset <- tryCatch({
               br <- attr(f, "breaks");
               if (length(br) > 0) {
                  if ("matrix" %in% class(attr(f, "colors"))) {
                     # previous version of circlize stored rgb matrix
                     nameVector(
                        grDevices::rgb(attr(f, "colors")),
                        format(digits=2, br))
                  } else {
                     # recent version of circlize stores hex values
                     nameVector(
                        attr(f, "colors"),
                        format(digits=2, br))
                  }
               } else {
                  attr(f, "colors")
               }
            }, error=function(e){
               NULL;
            })
         } else {
            colorset <- tryCatch({
               k <- seq_len(n)
               nameVector(f(n), k)
            }, error=function(e){
               NULL;
            })
         }
      } else if (is.character(f)) {
         colorset <- f
      } else {
         colorset <- NULL
      }
      colorset
   }

   if (igrepHas("list", class(x))) {
      # convert any entries from function to color vector
      x <- lapply(x, function(i){
         if (length(i) > 0 && is.function(i)) {
            fn_to_color(i)
         } else {
            i
         }
      });
      makeUnique <- rep(makeUnique,
         length.out=length(x));
      # optionally return only one unique color per list element
      if (TRUE %in% makeUnique) {
         x[makeUnique] <- lapply(x[makeUnique], function(i){
            tryCatch({
               i[!duplicated(i)]
            }, error=function(e){
               i
            })
         })
      }
      xM <- rbindList(x);
      colnames(xM) <- paste0("item_", padInteger(seq_len(ncol(xM))));
      if (length(labelCells)==0) {
         if (max(dim(xM)) > 40) {
            labelCells <- FALSE;
         } else {
            labelCells <- TRUE;
         }
      }
      if (TRUE %in% labelCells) {
         xMnames <- rbindList(lapply(x, function(i){
            if (is.null(names(i))) {
               rep("", length(i));
            } else {
               names(i);
            }
         }));
         if (length(xMnames) == 0) {
            xMnames <- xM;
         } else {
            colnames(xMnames) <- colnames(xM);
         }
      } else {
         xMnames <- xM;
         xMnames[] <- "";
      }
   } else {
      makeUnique <- head(makeUnique, 1);
      if (is.function(x)) {
         x <- fn_to_color(x);
         if (length(x) == 0) {
            return(invisible(x));
         }
      }
      # optionally return only one unique color per list element
      if (TRUE %in% makeUnique) {
         x <- tryCatch({
            x[!duplicated(x)]
         }, error=function(e){
            x
         })
      }
      xM <- matrix(x, nrow=1);
      if (!is.null(names(x))) {
         colnames(xM) <- names(x);
      }
      if (length(labelCells) == 0) {
         if (max(dim(xM)) > 40) {
            labelCells <- FALSE;
         } else {
            labelCells <- TRUE;
         }
      }
      if (TRUE %in% labelCells) {
         if (!is.null(names(x))) {
            xMnames <- matrix(names(x), nrow=1);
         } else {
            xMnames <- xM;
         }
      } else {
         xMnames <- xM;
         xMnames[] <- "";
      }
   }

   if (TRUE %in% adjustMargins &&
         length(colnames(xM)) == 0 &&
         length(rownames(xM)) == 0) {
      adjustMargins <- FALSE;
   }
   if (TRUE %in% doPlot) {
      if (TRUE %in% transpose) {
         if (adjustMargins) {
            ## Detect string width to adjust margins
            arglist <- list(...);
            use_xaxt <- ifelse(length(arglist$xaxt) > 0,
               arglist$xaxt, graphics::par("xaxt"))
            if (!"n" %in% use_xaxt) {
               withr::local_par(adjustAxisLabelMargins(x=rownames(xM),
                  margin=1,
                  ...))
            }
            use_yaxt <- ifelse(length(arglist$yaxt) > 0,
               arglist$yaxt, graphics::par("yaxt"))
            if (!"n" %in% use_yaxt) {
               withr::local_par(adjustAxisLabelMargins(x=colnames(xM),
                  margin=2,
                  ...))
            }
         }
         if (length(srtCellnote)==0) {
            if (nrow(xM) > ncol(xM)) {
               srtCellnote <- 90;
            } else {
               srtCellnote <- 0;
            }
         }
         imageByColors(t(xM),
            cellnote=t(xMnames),
            flip="y",
            srtCellnote=srtCellnote,
            ...);
      } else {
         if (adjustMargins) {
            ## Detect string width to adjust margins
            arglist <- list(...);
            use_xaxt <- ifelse(length(arglist$xaxt) > 0,
               arglist$xaxt, graphics::par("xaxt"))
            if (!"n" %in% use_xaxt) {
               withr::local_par(adjustAxisLabelMargins(x=colnames(xM),
                  margin=1,
                  ...))
            }
            use_yaxt <- ifelse(length(arglist$yaxt) > 0,
               arglist$yaxt, graphics::par("yaxt"))
            if (!"n" %in% use_yaxt) {
               withr::local_par(adjustAxisLabelMargins(x=rownames(xM),
                  margin=2,
                  ...))
            }
         }
         if (is.null(srtCellnote)) {
            if (nrow(xM) > ncol(xM)) {
               srtCellnote <- 0;
            } else {
               srtCellnote <- 90;
            }
         }
         imageByColors(xM,
            cellnote=xMnames,
            flip="y",
            srtCellnote=srtCellnote,
            ...);
      }
   }
   if (TRUE %in% transpose) {
      xM <- t(xM);
   }
   return(invisible(xM));
}
