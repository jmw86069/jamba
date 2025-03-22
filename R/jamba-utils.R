
# jamba random utilities

#' Lightweight method to check if an R package is installed
#'
#' Lightweight method to check if an R package is installed
#'
#' There are many methods to test for an installed package.
#' Most approaches incur some time or resource penalty, so
#' `check_pkg_installed()` is motivated for rapid results without
#' loading the package namespace.
#'
#' This function also accepts multiple values for `x` for convenience.
#'
#' There are two available methods defined by `useMethod`:
#'
#' 1. `useMethod="packagedir"` confirms:
#' this function represents possibly the most gentle and rapid
#' approach. It simply calls `system.file(package=x)`, for
#' each entry of `x`, then checks these requirements:
#'
#'    * Does the package directory exist via `system.file(package=x)`
#'    * Does the package directory contain the file 'DESCRIPTION'?
#'    * It does not check whether the package can be loaded into
#'    the current R session.
#'
#' 2. `useMethod="requireNamespace"` confirms:
#'
#'    * `requireNamespace(x, quietly=TRUE)` returns TRUE
#'    * It therefore loads the package namespace to confirm, but
#'    does not attach the package to the current session.
#'    It therefore may take time and resources, despite not
#'    altering the R environment search path.
#'
#' The default behavior first tests by "packagedir", then
#' for any `FALSE` results it also tests `"requireNamespace"`.
#'
#' @family jam practical functions
#'
#' @returns `logical` vector indicating whether each value in `x`
#'    represents an installed R package. The vector is named by
#'    packages provided in `x`.
#'
#' @param x `character` string of package or packages to test.
#' @param useMethod `character` default "packagedir" with the method
#'    of package confirmation.
#'    * "packagedir" provides a rapid test for the presence of an R
#'    package, without loading the package namespace.
#'    It tests whether `system.file(package=x)` returns  a non-empty value,
#'    then 'DESCRIPTION' file exists in the package directory.
#'    It answers the question: "Is 'x' package installed?"
#'    It does not answer: "Is 'x' package usable in the current R session?"
#'    When `useMethod` also includes "requireNamespace", for any FALSE
#'    result it will also perform a secondary check as well,
#'    to confirm the package cannot be loaded by another mechanism.
#'    * "requireNamespace" uses `requireNamespace(x, quietly=TRUE)`,
#'    with slight benefit that it accepts multiple values for `x`, and
#'    returns the result without using `invisible()`.
#'    This method loads the package namespace, but does not attach it.
#'    This method therefore takes the same time as loading the package,
#'    in return for providing the most accurate answer to the question:
#'    "Is 'x' package usable by this R session right now?"
#' @param ... additional arguments are ignored.
#'
#' @examples
#' check_pkg_installed("methods")
#'
#' check_pkg_installed(c("jamba",
#'    "multienrichjam",
#'    "venndir",
#'    "methods",
#'    "blah"))
#'
#' @export
check_pkg_installed <- function
(x,
 useMethod=c("packagedir",
    "requireNamespace"),
 ...)
{
   if (length(x) == 0) {
      return(NULL)
   }
   useMethod <- match.arg(useMethod,
      several.ok=TRUE);
   if ("packagedir" %in% head(useMethod, 1)) {
      pkg_installed <- sapply(x, function(i){
         pkgdir <- system.file(package=i);
         pkgdescfile <- file.path(pkgdir, "DESCRIPTION")
         (length(pkgdir) > 0 &&
            nchar(pkgdir) > 0 &&
            file.exists(pkgdescfile))
      })
      if (FALSE %in% pkg_installed && "requireNamespace" %in% useMethod) {
         pf <- (pkg_installed %in% FALSE)
         pkg_installed[pf] <- sapply(x[pf], function(i){
            requireNamespace(i, quietly=TRUE)
         })
      }
   } else {
      pkg_installed <- sapply(x, function(i){
         requireNamespace(i, quietly=TRUE)
      })
   }
   return(pkg_installed)
}

