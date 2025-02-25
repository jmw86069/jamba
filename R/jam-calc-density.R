
#' Calculate scatter plot point density
#'
#' Calculate scatter plot point density
#'
#' This function is called internally by `plotSmoothScatter()`,
#' and is an equivalent replacement for
#' `grDevices` non-exported function .smoothScatterCalcDensity(),
#' understandably a requirement by CRAN. A package should not rely
#' on another package hidden function.
#'
#' @param x `numeric` matrix with two columns representing x,y coordinates.
#' @param nbin `integer` number of bins to subdivide the scatterplot,
#'    expanded to length 2 to accommodate x and y axis bins.
#' @param bandwidth `numeric` or `NULL` representing the bandwidth used
#'    for point density determination.
#' @param range.x `numeric` vector length 2 representing the range of
#'    values to consider for point density.
#' @family jam internal functions
#'
#' @returns `list` with elements used internally by `plotSmoothScatter()`,
#'    with: x1, x2, fhat, bandwidth.
#'
#' @examples
#' sdim(jamCalcDensity(cbind(x=rnorm(1000) + 4, y=rnorm(1000) + 4), nbin=30))
#'
#' @export
jamCalcDensity <- function
(x,
 nbin,
 bandwidth=NULL,
 range.x)
{
   if (length(nbin) == 1) {
      nbin <- c(nbin, nbin)
   }
   if (!is.numeric(nbin) || length(nbin) != 2)
      stop("'nbin' must be numeric of length 1 or 2")
   if (length(bandwidth) == 0) {
      # quantile range of 5% to 95%, divided by 25
      bandwidth <- diff(apply(x, 2, stats::quantile,
         probs=c(0.05, 0.95),
         na.rm=TRUE,
         names=FALSE))/25;
      bandwidth[bandwidth == 0] <- 1
   } else {
      if (!is.numeric(bandwidth)) {
         stop("'bandwidth' must be numeric")
      }
      if (any(bandwidth <= 0)) {
         stop("'bandwidth' must be positive")
      }
   }
   rv <- KernSmooth::bkde2D(x,
      bandwidth=bandwidth,
      gridsize=nbin,
      range.x=range.x)
   rv$bandwidth <- bandwidth
   rv
}
