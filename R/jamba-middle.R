
#' Return the middle portion of data similar to head and tail
#'
#' Return the middle portion of data similar to head and tail
#'
#' This function is very simple, and is intended to mimic `head()`
#' and `tail()` to inspect data without looking at every value
#'
#' @family jam practical functions
#'
#' @returns an object of class equivalent to `x`.
#'
#' @param x input data that can be subset
#' @param n `numeric` number of entries to return
#' @param evenly `logical` indicating whether to return evenly spaced
#'    entries along the full length of `x`. When `evenly=FALSE` only
#'    the middle `n` entries are returned.
#' @param ... additional arguments are ignored.
#'
#' @examples
#' x <- 1:101;
#' middle(x);
#' middle(x, evenly=TRUE)
#'
#' xdf <- data.frame(n=1:101,
#'    excel_colname=jamba::colNum2excelName(1:101));
#' middle(xdf)
#' middle(xdf, evenly=TRUE)
#'
#' @export
middle <- function
(x,
 n=10,
 evenly=TRUE,
 ...)
{
   # similar to head() and tail() except pulls from middle
   # define appropriate length based upon input type
   if (length(nrow(x)) > 0) {
      x_len <- nrow(x);
   } else {
      x_len <- length(x)
   }
   # return input when n < x_len
   if (n >= x_len) {
      return(x)
   }


   if (TRUE %in% evenly) {
      k <- round(seq(from=1,
         to=x_len,
         length.out=n))
   } else {
      trim1 <- floor((x_len - n) / 2)
      k <- seq(from=trim1 + 1, to=trim1 + n, by=1)
   }
   if (length(nrow(x)) > 0) {
      x_return <- x[k, , drop=FALSE]
   } else {
      x_return <- x[k];
   }
   return(x_return)
}
