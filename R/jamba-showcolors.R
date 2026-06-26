

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
#' When input is `function`, it is assumed to be one of these
#' formats:
#' 
#' 1. `viridis::viridis(n)`
#' 
#'    * Sequential colors with `n` steps
#'    representing colors from lowest to highest value.
#'    * This format does not apply any numeric range,
#'    no numeric threshold is implied in the function at all.
#'    It simply takes its internal range of colors and produces
#'    `n` output colors, usually using `colorRampPalette()`
#'    to interpolate intermediate colors when needed.
#'    * Common examples are R packages such as 'viridis'
#'    which provide several different color gradients.
#'  
#' 2. `circlize::colorRamp2()`
#' 
#'    * Colors associated with specific `numeric` values.
#'    * A distinctive feature of `colorRamp2()` color `function`
#'    is that it assigns a vector of 'colors' to a vector
#'    of `numeric` values. This mechanism is important when
#'    it is useful to know exactly what `numeric` value
#'    is represented by a color.
#'    * A common alternative is to assign colors *between*
#'    `numeric` breaks, in which case the defined color
#'    is associated with an intermediate value betweenbreaks.
#' 
#' 3. `ggplot` or `ggproto` scales object produced by 'ggplot2'.
#' 
#'    * Given a `ggplot2::ggplot` object, it will use
#'    'colour' or 'fill' aesthetics present in the 'mapping',
#'    only when there is an explicit color assignment.
#'    It does not currently determine the default color
#'    aesthetic function to use. 
#'    * A `ggproto` object produced by a color scales function,
#'    for example `ggplot2::scale_color_discrete()`,
#'    for aesthetics 'color', 'colour', or 'fill'.
#'    * It makes reasonable attempt to recognize custom limits,
#'    for example '_gradientn()' functions which may have
#'    specific 'values' (scaled from 0 to 1) buthent applied to
#'    specific 'limits' (scaled per user coordinates).
#'
#' @family jam plot functions
#' @family jam color functions
#'
#' @param x one of these input types:
#'    * `character` vector of colors
#'    * `list` with any combination of `character` or `function`.
#'    Each element of the `list` is displayed on its own row.
#'    List names are shown on the y-axis.
#'    * `function`: color function in one of two formats:
#'       1. `circlize::colorRamp2()` which defines numeric breaks, and
#'       one color *at* each break. This function is used by ComplexHeatmap
#'       and is unique and useful in defining the color at each break and
#'       not in between each break. The benefit is that a specific color
#'       is known to mean exactly the numeric value, instead of assigning the
#'       color to some intermediate mean of adjacent breaks, then interpolating
#'       the color between them.
#'       2. `function` as defined in color packages such as `viridis::viridis(10)`
#'       where the number `10` defines the number of colors to produce.
#'       For these functions, colors are displayed ranging from 0 to 1,
#'       intending to mean lowest (0) to highest (1) color, with n steps.
#'    * 'ggplot' or 'ggproto' object, see Details.
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

   # wrapper to convert ggplot2 ScaleDiscrete or ScaleContinuous
   # to color
   ggplot2_scale_to_color <- function(gdc, ...)
   {
      if (inherits(gdc, "ScaleDiscrete")) {
         # named colors
         ggenv <- environment(environment(gdc$palette)$f);
         if ("values" %in% ls(ggenv)) {
            ggcolors <- ggenv$values;
            if (length(names(ggcolors)) == 0) {
               names(ggcolors) <- seq_along(ggcolors);
            } else {
               ggcolors <- ggcolors[mixedOrder(names(ggcolors))];
            }
         } else if ("fun" %in% ls(ggenv)) {
            ggcolorfun <- ggenv$fun
            ## nlevels is not accurate
            # if (length(n) == 0 && is.numeric(ggenv$nlevels)){
            #    n <- ggenv$nlevels;
            # }
            ggcolors <- rmNA(getColorRamp(col=ggcolorfun,
               # n=n,
               # gradientN=gradientN,
               ...));
            names(ggcolors) <- seq_along(ggcolors);
            # Todo: Consider attr(ggcolors, 'label')
            # with mash-up of ggplot2 information:
            # palette (character), type(character)
         } else{
            stop("ScaleDiscrete does not have 'values' nor 'fun'.");
         }
         ggcolors
      } else if (inherits(gdc, c("ScaleContinuous", "ScaleBinned"))) {
         if (!any(c("fill", "color", "colour") %in% gdc$aesthetics)) {
            # if not a fill or color, return NULL
            return(NULL)
         }
         ggenv1 <- environment(environment(gdc$palette)$f);
         # Sometimes palette is defined as a function,
         # otherwise try fallback_palette
         nseq <- seq(from=0, to=1, length.out=11);
         if (length(gdc$call$values) >= 2) {
            nseq_add <- eval(gdc$call$values);
            # Decision: Keep original nseq but make sure
            # the original values are represented
            nseq <- sort(unique(c(nseq, nseq_add)));
         }
         if (length(gdc$limits) == 2) {
            names(nseq) <- format(digits=2,
               normScale(nseq,
                  low=0,
                  high=1,
                  from=gdc$limits[1],
                  to=gdc$limits[2]))
         } else {
            names(nseq) <- format(digits=2, nseq);
         }
         if (length(gdc$palette) > 0) {
            # ggenv <- environment(environment(gdc$palette)$f);
            ggcolors <- gdc$palette(nseq);
            names(ggcolors) <- names(nseq);
         } else if (length(gdc$palette) == 0 &&
            length(gdc$fallback_palette) > 0) {
            nseq <- seq(from=0, to=1, length.out=11)
            ggcolors <- gdc$fallback_palette(nseq);
            names(ggcolors) <- names(nseq);
         } else{
            stop("ScaleContinuous not recognized.");
         }
         ggcolors;
         }
   }

   # wrapper to convert a function or ggplot2 object to colors
   fn_to_color <- function(f, n=7, ...) {
      if (is.function(f)) {
         # circlize::colorRamp2() output
         if (all(c("colors", "breaks") %in% names(attributes(f)))) {
            colorset <- tryCatch({
               # legend_at,legend_labels are custom attributes
               # defined in colorjam for divergent and linear colors
               # which may have a numeric floor.
               if (length(attr(f, "legend_at")) > 0) {
                  br <- attr(f, "legend_at");
                  brnames <- attr(f, "legend_labels");
                  if (length(brnames) != length(br)) {
                     brnames <- format(digits=2, br);
                  }
                  colorset1 <- f(br);
                  names(colorset1) <- brnames;
                  return(colorset1)
               }
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
      } else if (inherits(f, c("ScaleDiscrete",
         "ScaleContinuous",
         "ScaleBinned"))) {
         colorset <- ggplot2_scale_to_color(f, ...);
      } else {
         colorset <- NULL
      }
      colorset
   }

   #######################################
   ## Iterate input colors
   if (inherits(x, c("ggplot", "ggplot2::ggplot"))) {
      # Todo: Consider including only mapped aesthetics
      # Todo: Consider sniffing default aesthetics when
      #    not explicitly defined
      xmapped <- intersect(c("color", "colour", "fill"),
         names(x$mapping))
      if (length(xmapped) == 0) {
         return(NULL)
      }
      # # Todo: Consider including only aesthetics in a layer
      # xlayers <- x$layers;
      x <- x$scales$scales;
      xaes <- sapply(x, function(ix){
         ix$aesthetics;
      });
      xkeep <- xaes %in% xmapped;
      if (!any(xkeep)) {
         return(NULL)
      }
      x <- x[xkeep];
      # replace with legend name if defined
      xnames <- sapply(seq_along(x), function(ix){
         if (length(x[[ix]]$name) == 1 && nchar(x[[ix]]$name) > 0) {
            x[[ix]]$name
         } else {
            x[[ix]]$aesthetics
         }
      })
      names(x) <- makeNames(xnames);
   } else if (inherits(x, "ggproto")) {
      if (!any(c("color", "colour", "fill") %in% x$aesthetics)) {
         return(NULL)
      }
      x <- list(x);
      names(x) <- x[[1]]$aesthetics;
      # names(x) <- paste0(x[[1]]$aesthetics, "\n", head(class(x[[1]]), 1))
   }
   if (igrepHas("list", class(x))) {
      # convert any entries from function to color vector
      x <- lapply(x, function(i){
         if (length(i) > 0 &&
            (is.function(i) || !is.atomic(i))) {
            fn_to_color(i, ...)
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
      if (is.function(x) || !is.atomic(x)) {
         x <- fn_to_color(x, ...);
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
