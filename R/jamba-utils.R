
# jamba random utilities

#' Lightweight method to check if an R package is installed
#'
#' Lightweight method to check if an R package is installed
#'
#' There are many methods to test for an installed package,
#' this function represents possibly the most gentle and rapid
#' approach. It simply calls `system.file(package="")`,
#' which checks in the context of the active R session
#' and uses the relevant `.libPaths()`.
#'
#' This approach does not use `require()` because that actually
#' loads the package, which can take time and resources.
#'
#' This approach also does not use `installed.packages()`
#' which can also take substantial time if many packages
#' are installed on a system.
#'
#' @family jam practical functions
#'
#' @returns `logical` indicating whether each value in `x`
#'    represents an installed R package.
#'
#' @param x `character` string of package or packages to test.
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
 ...)
{
   if (length(x) == 0) {
      return(NULL)
   }
   sapply(x, function(i){
      nchar(system.file(package=i)) > 0
   })
}

