
#' remove NA values from list elements
#'
#' remove NA values from list elements
#'
#' This function removes `NA` values from vectors in a `list`,
#' applying the same logic used in `rmNA()` to each vector.
#' It is somewhat optimized, in that it checks for list elements
#' that have `NA` values before applying `rmNA()`.
#' However, it calls `rmNA()` iteratively on each vector that
#' contains `NA` in order to preserve the class
#' (factor, character, numeric, etc.) of each vector.
#'
#' It also optionally applies convenience functions `rmNULL()`
#' and `rmInfinite()` as relevant.
#'
#' @returns `list` where NA entries were removed or replaced with `naValue`
#'    in each vector. Empty `list` elements are optionally removed when
#'    `rmNULL=TRUE`, or replaced with `nullValue` when defined. When
#'    `rmInfinite=TRUE` then infinite values are either removed, or
#'    replaced with `infiniteValue` when defined.
#'
#' @family jam practical functions
#'
#' @param x `list` of vectors
#' @param naValue NULL or single replacement value for NA entries. If NULL,
#'    then NA entries are removed from the result.
#' @param rmNULL `logical` whether to replace NULL entries with `nullValue`
#' @param nullValue NULL or single replacement value for NULL entries. If NULL,
#'    then NULL entries are removed from the result.
#' @param rmInfinite `logical` whether to replace Infinite values with
#'    infiniteValue
#' @param infiniteValue value to use when rmInfinite==TRUE to replace
#'    entries which are Inf or -Inf.
#' @param rmNAnames `logical` whether to remove entries which have NA as the
#'    name, regardless whether the entry itself is NA.
#' @param verbose `logical` whether to print verbose output
#' @param ... additional arguments are ignored.
#'
#' @examples
#' testlist <- list(
#'    A=c(1, 4, 5, NA, 11),
#'    B=c("B", NA, "C", "Test"))
#' rmNAs(testlist)
#'
#' testlist2 <- list(
#'    A=c(1, 4, 5, NA, 11, Inf),
#'    B=c(11, NA, 19, -Inf))
#' rmNAs(testlist2, naValue=-100, infiniteValue=1000)
#'
#' @export
rmNAs <- function
(x,
 naValue=NULL,
 rmNULL=FALSE,
 nullValue=naValue,
 rmInfinite=TRUE,
 infiniteValue=NULL,
 rmNAnames=FALSE,
 verbose=FALSE,
 ...)
{
   # Purpose is a somewhat efficient method to remove NA values
   # from list object
   if (length(x) == 0) {
      if (rmNULL) {
         x <- nullValue;
      }
      return(x);
   }

   # check for any NA values in x
   xu <- unlist(x);
   if (any(is.na(xu))) {
      xidx <- rep(seq_along(x), lengths(x));
      # list index positions that contain NA values
      na_idx <- unique(xidx[is.na(xu)]);
      # remove NA
      for (idx in na_idx) {
         x[[idx]] <- rmNA(x[[idx]],
            naValue=naValue,
            rmInfinite=rmInfinite,
            infiniteValue=infiniteValue,
            rmNAnames=FALSE,
            verbose=FALSE)
      }
   }

   ## Note: NULL should only occur in lists, not vectors
   if (TRUE %in% rmNULL) {
      isNULL <- sapply(x, is.null);
      if (any(isNULL)) {
         x[isNULL] <- naValue;
      }
   }

   ## Optionally remove entries with NA names
   if (TRUE %in% rmNAnames && length(names(x)) > 0 && any(is.na(names(x)))) {
      keep_x <- which(!is.na(names(x)));
      x <- x[keep_x];
   }
   x;
}
