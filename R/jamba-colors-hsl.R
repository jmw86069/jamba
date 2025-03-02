
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
#' withr::with_par(list("mfrow"=c(4, 4), "mar"=c(0.2, 1, 4, 1)), {
#'
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
#' showColors(xaxt="n",
#'    list(
#'       hsl=hsl_gradient,
#'       hcl=hcl_gradient),
#'    main=paste0("Hue: ", round(H),
#'       "\nSat: ", S,
#'       "\nLum: (as labeled)"),
#'    groupCellnotes=FALSE)
#' }
#' })
#'
#' @family jam color functions
#'
#' @returns `numeric` matrix of H, S, L color values.
#'
#' @export
col2hsl <- function
(x,
 ...)
{
   ## Purpose is to convert R color to hsl
   ## R color can be a hex string or color name from grDevices::colors()
   if (!requireNamespace("farver", quietly=TRUE)) {
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
#' @param x `numeric` matrix of colors, with rownames `"H"`, `"S"`, `"L"`,
#'    or if not supplied it looks for vectors `H`, `S`, and `L` accordingly.
#' @param H,S,L `numeric` vectors supplied as an alternative to `x`, with
#'    ranges 0 to 360, 0 to 100, and 0 to 100, respectively.
#' @param alpha `numeric` vector of alpha values, default NULL.
#'    If not supplied, and if `x` is supplied as a matrix with
#'    rowname `"alpha"`, then values will be used from `x["alpha",]`.
#' @param verbose `logical` indicating whether to print verbose output.
#' @param ... other arguments are ignored.
#'
#' @returns vector of R colors, or where the input was NA, then NA
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
   if (!requireNamespace("farver", quietly=TRUE)) {
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
