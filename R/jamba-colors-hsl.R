
#' convert R color to HSL color matrix
#'
#' convert R color to HSL color matrix
#'
#' This function takes an R color and converts to an HSL matrix, using
#' the `farver` package `farver::decode_colour()`
#' the colorspace package, and \code{\link[colorspace]{RGB}} and
#' \code{\link[colorspace]{polarLUV}} functions. It is also used to
#' maintain alpha transparency, to enable interconversion via other
#' color manipulation functions as well.
#'
#' When `model="hsl"` this function uses `farver::decode_colour()`
#' and bypasses `colorspace`. In future the `colorspace` dependency
#' will likely be removed in favor of using `farver`. In any event,
#' `model="hsl"` is equivalent to using `model="polarLUV"` and
#' `fixup=TRUE`, except that it should be much faster.
#'
#' @param x `character` vector with R compatible colors.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' x <- c("#FF000044", "#FF0000", "firebrick");
#' names(x) <- x;
#' showColors(x)
#' xhsl <- col2hsl(x)
#' xhsl
#'
#' xhex <- hsl2col(xhsl)
#' showColors(list(x=x,
#'    xhex=xhex),
#'    groupCellnotes=FALSE)
#'
#' opar <- par("mfrow"=c(4, 4));
#' on.exit(par(opar));
#' for (H in seq(from=0, to=360, length.out=17)[-17]) {
#' S <- 75;
#' Lseq <- seq(from=15, to=95, by=10);
#' hsl_gradient <- hsl2col(
#'    H=H,
#'    S=85,
#'    L=Lseq);
#' hcl_gradient <- hcl2col(
#'    H=H,
#'    C=85,
#'    L=Lseq);
#' names(hsl_gradient) <- Lseq;
#' names(hcl_gradient) <- Lseq;
#' showColors(
#'    list(
#'       hsl=hsl_gradient,
#'       hcl=hcl_gradient),
#'    main=paste0("Hue: ", round(H),
#'       "\nSat: ", S,
#'       "\nLum: (as labeled)"),
#'    groupCellnotes=FALSE)
#' }
#'
#' @family jam color functions
#'
#' @export
col2hsl <- function
(x,
 ...)
{
   ## Purpose is to convert R color to hsl
   ## R color can be a hex string or color name from colors()
   if (!check_pkg_installed("farver")) {
      stop("The farver package is required.");
   }

   x3 <- t(farver::decode_colour(x,
      to="hsl",
      alpha=TRUE,
      ...));
   colnames(x3) <- names(x);
   rownames(x3)[1:3] <- toupper(rownames(x3)[1:3]);
   return(x3);
}


#' convert HCL to R color
#'
#' Convert an HCL color matrix to vector of R hex colors
#'
#' This function takes an HCL matrix,and converts to an R color using
#' the colorspace package `colorspace::polarLUV()` and `colorspace::hex()`.
#'
#' When `model="hcl"` this function uses `farver::encode_colour()`
#' and bypasses `colorspace`. In future the `colorspace` dependency
#' will likely be removed in favor of using `farver`. In any event,
#' `model="hcl"` is equivalent to using `model="polarLUV"` and
#' `fixup=TRUE`, except that it should be much faster.
#'
#' @param x matrix of colors, with rownames `"H"`, `"C"`, `"L"`, or if not
#'    supplied it looks for vectors `H`, `C`, and `L` accordingly. It can
#'    alternatively be supplied as an object of class `polarLUV`.
#' @param H,C,L numeric vectors supplied as an alternative to `x`, with
#'    ranges 0 to 360, 0 to 100, and 0 to 100, respectively.
#' @param maxColorValue numeric value indicating the maximum RGB values,
#'    typically scaling values to a range of 0 to 255, from the default
#'    returned range of 0 to 1. In general, this value should not be
#'    modified.
#' @param ceiling numeric value indicating the maximum values allowed for
#'    `R`, `G`, and `B` after conversion by `colorspace::as(x, "RGB")`.
#'    This ceiling is applied after the `maxColorValue` is used to scale
#'    numeric values, and is intended to correct for the occurrence of
#'    values above 255, which would be outside the typical color gamut
#'    allowed for RGB colors used in R. In general, this value should not
#'    be modified.
#' @param alpha optional vector of alpha values. If not supplied, and if
#'    `x` is supplied as a matrix with rowname `"alpha"`, then values will
#'    be used from `x["alpha",]`.
#' @param fixup boolean indicating whether to use
#'    `colorspace::hex(...,fixup=TRUE)` for conversion to R hex colors,
#'    **which is not recommended** since this conversion applies some
#'    unknown non-linear transformation for colors outside the color gamut.
#'    It is here is an option for comparison, and if specifically needed.
#' @param ... other arguments are ignored.
#'
#' @return vector of R colors, or where the input was NA, then NA
#'    values are returned in the same order.
#'
#' @examples
#' # See col2hcl() for more extensive examples
#'
#' # Prepare a basic HSL matrix
#' x_colors <- c(red="red",
#'    blue="blue",
#'    yellow="yellow",
#'    orange="#FFAA0066");
#' hslM <- col2hsl(x_colors);
#' hslM;
#'
#' # Now convert back to R hex colors
#' colorV <- hsl2col(hslM);
#' colorV;
#'
#' showColors(list(x_colors=x_colors,
#'    colorV=nameVector(colorV)));
#'
#' @family jam color functions
#'
#' @export
hsl2col <- function
(x=NULL,
 H=NULL,
 S=NULL,
 L=NULL,
 alpha=NULL,
 verbose=FALSE,
 ...)
{
   ## Purpose is to convert HSL back to an R hex color string
   if (!check_pkg_installed("farver")) {
      stop("The farver package is required.");
   }

   if (igrepHas("matrix", class(x))) {
      if (!all(c("H", "S", "L") %in% rownames(x))) {
         stop("hsl2col() requires matrix x with rownames H, S, L; or vectors H, S, and L.");
      }
      if (!"alpha" %in% rownames(x)) {
         if (length(alpha) == 0) {
            alpha <- 1;
         }
         x <- rbind(x,
            alpha=rep(alpha, ncol(x)))
      } else if (length(alpha) > 0) {
         x <- rbind(x,
            alpha=rep(alpha, ncol(x)))
      }
      xnames <- colnames(x);
   } else {
      if (length(H) == 0 ||
            length(S) == 0 ||
            length(L) == 0) {
         stop("hsl2col() requires matrix x with rownames H, S, L; or vectors H, S, and L.");
      }
      if (length(alpha) == 0) {
         alpha <- 1;
      }
      x <- rbind(H=H,
         S=S,
         L=L,
         alpha=alpha);
      xnames <- names(H);
   }
   if (verbose) {
      printDebug("hsl2col(): ",
         "head(x, 10):");
      print(head(x, 10));
   }

   # convert to HSL using farver
   xCol <- farver::encode_colour(
      t(x[c("H", "S", "L"), , drop=FALSE]),
      alpha=x["alpha", ],
      from="hsl");
   names(xCol) <- xnames;

   xCol;
}
