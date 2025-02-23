
# Quick and dirty rainbow improvement

#' Simple rainbow palette replacement
#'
#' Simple rainbow palette replacement using variable saturation and vibrance
#'
#' @returns `character` vector of R colors.
#'
#' @param n `integer` number of colors requested
#' @param s,v `numeric` vector of values to recycle as saturation and
#'    vibrance, respectively. The purpose is to improve visual distinction
#'    between adjacent and nearby colors in the color wheel.
#' @param ... additional arguments are passed to `grDevices::rainbow()`:
#'    * `start`,`end` to control the starting and ending hue `[0,1]`,
#'    * `alpha` for alpha opacity, default NULL adds no alpha,
#'    * `rev` to reverse the color order.
#'
#' @family jam color functions
#'
#' @examples
#' showColors(list(
#'    `rainbow(24)`=grDevices::rainbow(24),
#'    `rainbow2(24)`=rainbow2(24),
#'    `rainbow2(24, rev=TRUE)`=rainbow2(24, rev=TRUE),
#'    `rainbow2(24, start=0.5, end=0.499)`=rainbow2(24,
#'       start=0.5, end=0.5-1e-5),
#'    `rainbow2(24, rev=TRUE,\nstart=0.5, end=0.499)`=rainbow2(24,
#'       rev=TRUE, start=0.5, end=0.5-1e-5)))
#'
#' @export
rainbow2 <- function
(n,
 s=c(0.9, 0.7, 0.88, 0.55),
 v=c(0.92, 1, 0.85, 0.94),
 ...)
{
   #
   n <- head(n, 1);
   if (length(n) == 0 || n == 0) {
      return(NULL)
   }
   if (length(s) == 0) {
      s <- c(1, 0.7, 0.85, 0.55);
   }
   if (length(v) == 0) {
      v <- c(0.92, 1, 0.85, 0.94);
   }
   svals <- rep(s, length.out=n)
   vvals <- rep(v, length.out=n)

   new_rainbow <- grDevices::rainbow(n=n,
      s=svals,
      v=vvals,
      ...)
}
