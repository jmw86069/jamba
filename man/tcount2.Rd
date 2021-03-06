% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba.r
\name{tcount2}
\alias{tcount2}
\title{frequency of entries, ordered by frequency, minimum count 2}
\usage{
tcount2(
  x,
  minCount = 2,
  doSort = TRUE,
  maxCount = NULL,
  nameSortFunc = sort,
  ...
)
}
\arguments{
\item{x}{vector input to use when calculating frequencies.}

\item{minCount}{optional integer minimum frequency, any results with
fewer counts observed will be omitted from results. Note the default
for \code{tcount2()} is \code{minCount=2}.}

\item{doSort}{logical whether to sort results decreasing by frequency.}

\item{maxCount}{optional integer maximum frequency for returned results.}

\item{nameSortFunc}{function used to sort results after sorting by
frequency. For example, one might use \code{mixedSort()}. If
\code{nameSortFunc=NULL} then no name sort will be applied.}

\item{...}{additional parameters are ignored.}
}
\value{
integer vector of counts, named by the unique input
values in \code{x}, by default limited to entries with frequency
\code{2} or higher.
}
\description{
frequency of entries, ordered by frequency, minimum count 2
}
\details{
This function is a simple customization of \code{tcount()}
with \code{minCount=2} so it only reports frequencies of \code{2} or higher.
}
\examples{
testVector <- rep(c("one", "two", "three", "four"), c(1:4));
tcount(testVector);
tcount2(testVector);

}
\seealso{
Other jam string functions: 
\code{\link{asSize}()},
\code{\link{breaksByVector}()},
\code{\link{cPasteSU}()},
\code{\link{cPasteS}()},
\code{\link{cPasteUnique}()},
\code{\link{cPasteU}()},
\code{\link{cPaste}()},
\code{\link{fillBlanks}()},
\code{\link{formatInt}()},
\code{\link{gsubOrdered}()},
\code{\link{makeNames}()},
\code{\link{mixedOrder}()},
\code{\link{mixedSortDF}()},
\code{\link{mixedSorts}()},
\code{\link{mixedSort}()},
\code{\link{mmixedOrder}()},
\code{\link{nameVectorN}()},
\code{\link{nameVector}()},
\code{\link{padInteger}()},
\code{\link{padString}()},
\code{\link{pasteByRowOrdered}()},
\code{\link{pasteByRow}()},
\code{\link{tcount}()},
\code{\link{ucfirst}()},
\code{\link{uniques}()}
}
\concept{jam string functions}
