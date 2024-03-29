% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba-middle.R
\name{middle}
\alias{middle}
\title{Return the middle portion of data similar to head and tail}
\usage{
middle(x, n = 10, evenly = TRUE, ...)
}
\arguments{
\item{x}{input data that can be subset}

\item{n}{\code{numeric} number of entries to return}

\item{evenly}{\code{logical} indicating whether to return evenly spaced
entries along the full length of \code{x}. When \code{evenly=FALSE} only
the middle \code{n} entries are returned.}

\item{...}{additional arguments are ignored.}
}
\description{
Return the middle portion of data similar to head and tail
}
\details{
This function is very simple, and is intended to mimic \code{head()}
and \code{tail()} to inspect data without looking at every value
}
\examples{
x <- 1:101;
middle(x);
middle(x, evenly=TRUE)

xdf <- data.frame(n=1:101,
   excel_colname=jamba::colNum2excelName(1:101));
middle(xdf)
middle(xdf, evenly=TRUE)

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
\code{\link{lldf}()},
\code{\link{log2signed}()},
\code{\link{make_html_styles}()},
\code{\link{make_styles}()},
\code{\link{match_unique}()},
\code{\link{mergeAllXY}()},
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
