% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba-clrange.R
\name{checkLightMode}
\alias{checkLightMode}
\title{check lightMode for light background color}
\usage{
checkLightMode(lightMode = NULL, ...)
}
\arguments{
\item{lightMode}{\code{logical} or NULL, indicating whether the lightMode
parameter has been defined in the function call.}

\item{...}{Additional arguments are ignored.}
}
\value{
logical or length=1, indicating whether lightMode is defined
}
\description{
check lightMode for light background color
}
\details{
Check the lightMode status through function parameter, options, or
environment variable. If the function defines lightMode, it is used as-is.
If lightMode is NULL, then options("jam.lightMode") is used if defined.
Otherwise, it tries to detect whether the R session is running inside
Rstudio using the environmental variable "RSTUDIO", and if so it assumes
lightMode==TRUE.

To set a default lightMode, add options("jam.lightMode"=TRUE) to .Rprofile, or
to the relevant R script.
}
\examples{
checkLightMode(TRUE);
checkLightMode();

}
\seealso{
Other jam practical functions: 
\code{\link{breakDensity}()},
\code{\link{call_fn_ellipsis}()},
\code{\link{check_pkg_installed}()},
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
