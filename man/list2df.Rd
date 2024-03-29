% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba-string.r
\name{list2df}
\alias{list2df}
\title{Convert list of vectors to data.frame with item, value, name}
\usage{
list2df(x, makeUnique = TRUE, useVectorNames = TRUE, ...)
}
\arguments{
\item{x}{list of vectors}

\item{makeUnique}{logical indicating whether the data.frame should
contain unique rows.}

\item{useVectorNames}{logical indicating whether vector names should
be included in the data.frame, if they exist.}

\item{...}{additional arguments are ignored.}
}
\description{
Convert list of vectors to data.frame with item, value, name
}
\details{
This function converts a list of vectors to a tall data.frame
with colnames \code{item} to indicate the list name, \code{value} to indicate
the vector value, and \code{name} to indicate the vector name if
\code{useVectorNames=TRUE} and if names exist.
}
\seealso{
Other jam list functions: 
\code{\link{cPasteSU}()},
\code{\link{cPasteS}()},
\code{\link{cPasteUnique}()},
\code{\link{cPasteU}()},
\code{\link{cPaste}()},
\code{\link{heads}()},
\code{\link{jam_rapply}()},
\code{\link{mergeAllXY}()},
\code{\link{mixedSorts}()},
\code{\link{rbindList}()},
\code{\link{relist_named}()},
\code{\link{rlengths}()},
\code{\link{sclass}()},
\code{\link{sdim}()},
\code{\link{uniques}()},
\code{\link{unnestList}()}
}
\concept{jam list functions}
