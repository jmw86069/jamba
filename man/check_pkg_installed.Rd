% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba-utils.R
\name{check_pkg_installed}
\alias{check_pkg_installed}
\title{Lightweight method to check if an R package is installed}
\usage{
check_pkg_installed(x, ...)
}
\arguments{
\item{x}{\code{character} string of package or packages to test.}

\item{...}{additional arguments are ignored.}
}
\value{
\code{logical} indicating whether each value in \code{x}
represents an installed R package.
}
\description{
Lightweight method to check if an R package is installed
}
\details{
There are many methods to test for an installed package,
this function represents possibly the most gentle and rapid
approach. It simply calls \code{system.file(package="")},
which checks in the context of the active R session
and uses the relevant \code{.libPaths()}.

This approach does not use \code{require()} because that actually
loads the package, which can take time and resources.

This approach also does not use \code{installed.packages()}
which can also take substantial time if many packages
are installed on a system.
}
\examples{
check_pkg_installed("methods")

check_pkg_installed(c("jamba",
   "multienrichjam",
   "venndir",
   "methods",
   "blah"))

}
\seealso{
Other jam practical functions: 
\code{\link{breakDensity}()},
\code{\link{call_fn_ellipsis}()},
\code{\link{checkLightMode}()},
\code{\link{colNum2excelName}()},
\code{\link{color_dither}()},
\code{\link{exp2signed}()},
\code{\link{getAxisLabel}()},
\code{\link{isFALSEV}()},
\code{\link{isTRUEV}()},
\code{\link{jargs}()},
\code{\link{kable_coloring}()},
\code{\link{lldf}()},
\code{\link{log2signed}()},
\code{\link{middle}()},
\code{\link{minorLogTicks}()},
\code{\link{newestFile}()},
\code{\link{printDebug}()},
\code{\link{reload_rmarkdown_cache}()},
\code{\link{renameColumn}()},
\code{\link{rmInfinite}()},
\code{\link{rmNA}()},
\code{\link{rmNAs}()},
\code{\link{rmNULL}()},
\code{\link{setPrompt}()}
}
\concept{jam practical functions}
