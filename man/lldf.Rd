% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba-lldf.R
\name{lldf}
\alias{lldf}
\title{Long listing of R session objects}
\usage{
lldf(
  n = Inf,
  envir = .GlobalEnv,
  items = NULL,
  use_utils_objectsize = TRUE,
  all.names = TRUE,
  ...
)
}
\arguments{
\item{n}{\code{integer} or \code{Inf} indicating how many objects to
include in the output \code{data.frame}.}

\item{envir}{\code{environment} where the list of objects is obtained.
Note this environment is also where objects are evaluated,
so the \code{envir} and \code{items} should be compatible.}

\item{items}{optional \code{character} vector of items to include
in the output \code{data.frame}. Note that these items should be
available in the environment \code{envir}.}

\item{all.names}{\code{logical} passed to \code{base::ls()} indicating whether
to include all names, where \code{all.names=TRUE} will include
hidden objects whose name begin with \code{"."} such as \code{".First"}.}

\item{...}{additional arguments are passed to \code{ls()}}
}
\description{
Long listing of R session objects
}
\details{
This function expands \code{base::ls()} by also determining the
object size, and sorting to display the top \code{n} objects by
size, largest first.

This package will call \code{pryr::object_size} if available,
otherwise falls back to \code{utils::object.size()}.
}
\examples{
lldf(10);

# custom environment
newenv <- new.env();
newenv$A <- 1:10;
newenv$df <- data.frame(A=1:10, B=11:20);
lldf(envir=newenv);
rm(newenv);

}
\seealso{
Other jam practical functions: 
\code{\link{breakDensity}()},
\code{\link{checkLightMode}()},
\code{\link{check_pkg_installed}()},
\code{\link{colNum2excelName}()},
\code{\link{color_dither}()},
\code{\link{diff_functions}()},
\code{\link{exp2signed}()},
\code{\link{fileInfo}()},
\code{\link{fixYellow}()},
\code{\link{getAxisLabel}()},
\code{\link{handleArgsText}()},
\code{\link{heads}()},
\code{\link{isFALSEV}()},
\code{\link{isTRUEV}()},
\code{\link{jamba}},
\code{\link{jargs}()},
\code{\link{kable_coloring}()},
\code{\link{log2signed}()},
\code{\link{make_html_styles}()},
\code{\link{make_styles}()},
\code{\link{match_unique}()},
\code{\link{mergeAllXY}()},
\code{\link{middle}()},
\code{\link{minorLogTicks}()},
\code{\link{newestFile}()},
\code{\link{printDebug}()},
\code{\link{renameColumn}()},
\code{\link{rmInfinite}()},
\code{\link{rmNAs}()},
\code{\link{rmNA}()},
\code{\link{rmNULL}()},
\code{\link{sclass}()},
\code{\link{sdim}()},
\code{\link{setPrompt}()}
}
\concept{jam practical functions}
