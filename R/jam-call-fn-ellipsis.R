
#' Safely call a function using ellipsis
#'
#' Safely call a function using ellipsis
#'
#' This function is a wrapper function intended to help
#' pass ellipsis arguments `...` from a parent function
#' to an external function in a safe way.
#' 
#' 1. If the target function itself accepts '...' then all
#' arguments are passed.
#' 2. If the target function does not accept '...', then
#' only the arguments whose names are defined in `formals()`
#' will be passed along. All other arguments are not passed to
#' the target function.
#' 3. As of version 1.0.5, when there are duplicated argument
#' names in the input '...', only the first instance of each
#' argument name is passed along, while also applying rules
#' 1 and 2 as described above.
#'
#' Note that all arguments in the input '...' must be named.
#' 
#' ## Duplicate arguments as default values
#' 
#' When providing a 'default value' for an argument, consider
#' a function with arguments 'x' and 'y'. We may want to use
#' 'y=2' as a default value for 'y', without providing 'y'
#' as a formal argument in the wrapper function. By accepting
#' '...' the wrapper function can be simplified by having fewer
#' named arguments.
#' 
#' We have two options for handling the default 'y=2'.
#' 
#' First, we may permit the user to supply a custom value to
#' be used as priority when available. In this case, place
#' '...' before the 'y=2' default argument, as shown below:
#' 
#' ```
#' test_fn <- function(x, y, ...) { x + y }
#' wrapper <- function(x, ...) {
#'    call_fn_ellipsis(test_fn,
#'       ...,
#'       x=x,
#'       y=2)
#' }
#' wrapper(x=1, y=4)
#' #> output:
#' #> 5
#' ```
#' 
#' In this case (above), the user-supplied 'y=4' will be accepted
#' before the default 'y=2', and therefore the output will be  
#' 
#' `1 + 4 = 5`
#' 
#' The second option is to force the default 'y=2' to be used,
#' thereby ignoring the user-defined 'y=4'. In this case,
#' place '...' after the 'y=2' default argument value.
#'
#' ```
#' test_fn <- function(x, y, ...) { x + y }
#' wrapper2 <- function(x, ...) {
#'    call_fn_ellipsis(test_fn,
#'       x=x,
#'       y=2,
#'       ...)
#' }
#' wrapper2(x=1, y=4)
#' #> output:
#' #> 3
#' ```
#' 
#' In this case (above) the 'y=2' appears before the user-defined
#' 'y=4' in the argument stack, therefore 'y=2' takes priority.
#' The output will be  
#' 
#' `1 + 2 = 3`
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

   # 1.0.5 - remove duplicates when present
   arglist <- list(...)
   if (any(duplicated(names(arglist)))) {
      dupe_args <- names(tcount(names(arglist), 2));
      # emit a warning() which can be blocked if needed
      warning_msg <- paste0(
         "Duplicate argument names: ",
         cPaste(dupe_args, sep=", "));
      warning(warning_msg);
      arglist <- arglist[!duplicated(names(arglist))]
   }
   
   # apply to the function
   if ("..." %in% FUN_argnames) {
      do.call(FUN, arglist)
      # FUN(...)
   } else {
      # keep only args that match the function
      argkeep <- which(names(arglist) %in% FUN_argnames);
      arguse <- arglist[argkeep]
      do.call(FUN, arguse)
   }
}
