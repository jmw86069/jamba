
#' Long listing of R session objects
#'
#' Long listing of R session objects
#'
#' This function expands `base::ls()` by also determining the
#' object size, and sorting to display the top `n` objects by
#' size, largest first.
#'
#' This package will call `pryr::object_size` if available,
#' otherwise falls back to `utils::object.size()`.
#'
#' @family jam practical functions
#'
#' @returns `data.frame` with summary of objects and object sizes,
#'    sorted by decreasing object size.
#'
#' @param n `integer` or `Inf` indicating how many objects to
#'    include in the output `data.frame`.
#' @param envir `environment` where the list of objects is obtained.
#'    Note this environment is also where objects are evaluated,
#'    so the `envir` and `items` should be compatible.
#' @param items optional `character` vector of items to include
#'    in the output `data.frame`. Note that these items should be
#'    available in the environment `envir`.
#' @param use_utils_objectsize `logical`, default TRUE, whether to prefer
#'    `utils::object.size()`, otherwise it will attempt to use
#'    `pryr::object_size()` if the package is installed.
#' @param all.names `logical` passed to `base::ls()` indicating whether
#'    to include all names, where `all.names=TRUE` will include
#'    hidden objects whose name begin with `"."` such as `".First"`.
#' @param ... additional arguments are passed to `ls()`
#'
#' @examples
#' lldf(10);
#'
#' # custom environment
#' newenv <- new.env();
#' newenv$A <- 1:10;
#' newenv$df <- data.frame(A=1:10, B=11:20);
#' lldf(envir=newenv);
#' rm(newenv);
#'
#' @export
lldf <- function
(n=Inf,
 envir=.GlobalEnv,
 items=NULL,
 use_utils_objectsize=TRUE,
 all.names=TRUE,
 ...)
{
   # light check for pryr package without using require()
   if (!use_utils_objectsize &&
         requireNamespace("pryr", quietly=TRUE)) {
      osfun <- pryr::object_size;
   } else {
      osfun <- utils::object.size;
   }
   if (length(n) == 0) {
      n <- Inf;
   }
   if (length(items) == 0) {
      items <- ls(envir=envir,
         all.names=all.names,
         sorted=FALSE,
         ...)
   }

   df <- jamba::rbindList(lapply(items, function(i){
      data.frame(name=i,
         class=jamba::cPaste(class(get(i, envir=envir)), sep=", "),
         bytes=as.numeric(osfun(get(i, envir=envir)))
      )
   }));
   df <- head(jamba::mixedSortDF(df, byCols="-bytes"), n)
   rownames(df) <- df$name;
   # convert bytes to human friendly units
   df$size <- jamba::asSize(df$bytes);
   df;
}
