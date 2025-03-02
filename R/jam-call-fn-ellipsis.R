
#' Safely call a function using ellipsis
#'
#' Safely call a function using ellipsis
#'
#' This function is a wrapper function intended to help
#' pass ellipsis arguments `...` from a parent function
#' to an external function in a safe way. It will only
#' include arguments from `...` that are recognized by
#' the external function.
#'
#' The logic is described as follows:
#'
#' * When the external function `FUN` arguments `formals()` include
#' ellipsis `...`, then the ellipsis `...` will be passed as-is without
#' change. In this way, any arguments inside the original ellipsis `...`
#' will either match arguments in `FUN`, or will be ignored in that
#' function ellipsis `...`.
#' * When the external function `FUN` arguments `formals()` do not
#' include ellipsis `...`, then named arguments in `...` are passed
#' to `FUN` only when the arguments names are recognized by `FUN`.
#'
#' Note that arguments therefore must be named.
#'
#' @family jam practical functions
#'
#' @returns output from `FUN()` when called with relevant named arguments
#'    from ellipsis `...`
#'
#' @param FUN `function` that should be called with arguments in `...`
#' @param ... arguments are passed to `FUN()` in safe manner.
#'
#' @examples
#' new_mean <- function(x, trim=0, na.rm=FALSE) {
#'    mean(x, trim=trim, na.rm=na.rm)
#' }
#' x <- c(1, 3, 5, NA);
#' new_mean(x, na.rm=TRUE);
#' # throws an error as expected (below)
#' tryCatch({
#'    new_mean(x, na.rm=TRUE, color="red")
#' }, error=function(e){
#'    print("Error is expected, shown below:");
#'    print(e)
#' })
#'
#' call_fn_ellipsis(new_mean, x=x, na.rm=TRUE, color="red")
#'
#' @export
call_fn_ellipsis <- function
(FUN,
 ...)
{
   FUN_argnames <- names(formals(FUN));
   if ("..." %in% FUN_argnames) {
      FUN(...)
   } else {
      arglist <- list(...)
      argkeep <- which(names(arglist) %in% FUN_argnames);
      arguse <- arglist[argkeep]
      do.call(FUN, arguse)
   }
}
