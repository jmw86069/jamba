
#' Make dithered color pattern light-dark
#'
#' Make dithered color pattern light-dark
#'
#' This function serves a very simple purpose, mainly for
#' `printDebug()` to use subtle alternating light/dark colors
#' for vector output. It takes a color and returns two colors
#' which are slightly lighter and darker than each other,
#' to a minimum contrast defined by `colorspace::contrast_ratio()`.
#'
#' @returns format defined by argument `returnType`:
#'    * `vector`: two colors for every input color in `x`
#'    * `matrix`: two rows, input colors on first row, output colors
#'    on second row
#'    * `list`: a `list` with two colors in each element, with input
#'    and output colors together in each vector.
#'
#' @param x `character` vector of R colors
#' @param L_diff `numeric` value added or subtracted from the L in HSL
#'    color space for each color, until contrast is at least `min_contrast`.
#' @param min_contrast `numeric` minimum contrast as defined by
#'    `colorspace::contrast_ratio()` for the input and potential output
#'    color.
#' @param direction `numeric` that defines the initial direction,
#'    where values >= 0 start by making colors lighter, and values < 0
#'    make colors darker.
#' @param L_max,L_min `numeric` values that define the permitted range
#'    of L values in HSL color space, which ranges from 0 to 100.
#' @param returnType `character` string that defines the output of this
#'    function:
#'    * `vector`: two colors for every input color in `x`
#'    * `matrix`: two rows, input colors on first row, output colors
#'    on second row
#'    * `list`: a `list` with two colors in each element, with input
#'    and output colors together in each vector.
#' @param debug `logical` indicating whether to plot the color iterations
#'    using `showColors()`.
#' @param ... additional arguments are ignored.
#'
#' @family jam practical functions
#'
#' @examples
#' x <- "firebrick1";
#' showColors(color_dither(x))
#'
#' showColors(color_dither(x, direction=-1))
#'
#' x <- vigrep("^green[0-9]", grDevices::colors())
#' showColors(color_dither(x))
#' showColors(color_dither(x, direction=-1, returnType="list"))
#'
#' x <- c("green1", "cyan", "blue", "red", "gold", "yellow", "pink")
#' showColors(color_dither(x))
#'
#' color_dither(x, debug=TRUE)
#'
#' @export
color_dither <- function
(x,
 L_diff=4,
 L_max=90,
 L_min=30,
 min_contrast=1.25,
 direction=1,
 returnType=c("vector",
    "list",
    "matrix"),
 debug=FALSE,
 ...)
{
   returnType <- match.arg(returnType);
   if (length(x) == 0) {
      return(x);
   }
   xhsl <- col2hsl(x);
   L_ceiling <- (L_max - L_diff);
   L_floor <- (L_min + L_diff);
   direction <- rep(direction,
      length.out=length(x));
   L_add <- ifelse(direction >= 0,
      ifelse(xhsl["L",] > L_ceiling,
         -L_diff,
         L_diff),
      ifelse(xhsl["L",] < L_floor,
         L_diff,
         -L_diff))
   xhsl["L",] <- noiseFloor(xhsl["L",] + L_add,
      minimum=L_min,
      ceiling=L_max);
   xnew <- hsl2col(xhsl);
   xcontrast <- sapply(seq_along(x), function(i){
      colorspace::contrast_ratio(x[i],
         xnew[i])
   })
   n_iter <- 0;
   xlist <- list(x,
      nameVector(xnew, format(xcontrast, digits=3, trim=TRUE), makeNamesFunc=c));
   while(any(xcontrast < min_contrast) & n_iter < 100) {
      n_iter <- n_iter + 1;
      xadj <- (xcontrast < min_contrast);
      xflip <- (xhsl["L", xadj] %in% c(L_max, L_min));
      if (any(xflip)) {
         L_add[xflip] <- L_add[xflip] * -1;
      }
      xhsl["L", xadj] <- noiseFloor(
         xhsl["L",xadj] + L_add[xadj],
         minimum=L_min,
         ceiling=L_max);
      xnew[xadj] <- hsl2col(xhsl[,xadj,drop=FALSE]);
      xcontrast[xadj] <- sapply(which(xadj), function(i){
         colorspace::contrast_ratio(x[i],
            xnew[i])
      })
      xlist <- c(xlist,
         list(nameVector(xnew, format(xcontrast, digits=3, trim=TRUE), makeNamesFunc=c)))
   }
   if (debug) {
      showColors(c(final=tail(xlist, 1),
         input=head(xlist, 1),
         tail(xlist, -1)))
   }
   xm <- rbind(x, xnew);
   if ("matrix" %in% returnType) {
      return(xm);
   } else if ("vector" %in% returnType) {
      return(as.vector(xm))
   }
   xl <- unname(as.list(data.frame(check.names=FALSE, xm)));
   if (length(names(x)) > 0) {
      names(xl) <- names(x);
   }
   return(xl);
}

