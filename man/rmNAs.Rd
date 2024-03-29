% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba-rmnas.R
\name{rmNAs}
\alias{rmNAs}
\title{remove NA values from list elements}
\usage{
rmNAs(
  x,
  naValue = NULL,
  rmNULL = FALSE,
  nullValue = naValue,
  rmInfinite = TRUE,
  infiniteValue = NULL,
  rmNAnames = FALSE,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{x}{\code{list} of vectors}

\item{naValue}{NULL or single replacement value for NA entries. If NULL,
then NA entries are removed from the result.}

\item{rmNULL}{\code{logical} whether to replace NULL entries with \code{nullValue}}

\item{nullValue}{NULL or single replacement value for NULL entries. If NULL,
then NULL entries are removed from the result.}

\item{rmInfinite}{\code{logical} whether to replace Infinite values with
infiniteValue}

\item{infiniteValue}{value to use when rmInfinite==TRUE to replace
entries which are Inf or -Inf.}

\item{rmNAnames}{\code{logical} whether to remove entries which have NA as the
name, regardless whether the entry itself is NA.}

\item{verbose}{\code{logical} whether to print verbose output}

\item{...}{additional arguments are ignored.}
}
\value{
\code{list} where NA entries were removed or replaced with \code{naValue}
in each vector. Empty \code{list} elements are optionally removed when
\code{rmNULL=TRUE}, or replaced with \code{nullValue} when defined. When
\code{rmInfinite=TRUE} then infinite values are either removed, or
replaced with \code{infiniteValue} when defined.
}
\description{
remove NA values from list elements
}
\details{
This function removes \code{NA} values from vectors in a \code{list},
applying the same logic used in \code{rmNA()} to each vector.
It is somewhat optimized, in that it checks for list elements
that have \code{NA} values before applying \code{rmNA()}.
However, it calls \code{rmNA()} iteratively on each vector that
contains \code{NA} in order to preserve the class
(factor, character, numeric, etc.) of each vector.

It also optionally applies convenience functions \code{rmNULL()}
and \code{rmInfinite()} as relevant.
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
\code{\link{middle}()},
\code{\link{minorLogTicks}()},
\code{\link{newestFile}()},
\code{\link{printDebug}()},
\code{\link{renameColumn}()},
\code{\link{rmInfinite}()},
\code{\link{rmNA}()},
\code{\link{rmNULL}()},
\code{\link{sclass}()},
\code{\link{sdim}()},
\code{\link{setPrompt}()}
}
\concept{jam practical functions}
