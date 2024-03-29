% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba-string.r
\name{formatInt}
\alias{formatInt}
\title{Format an integer as a string}
\usage{
formatInt(
  x,
  big.mark = ",",
  trim = TRUE,
  forceInteger = TRUE,
  scientific = FALSE,
  ...
)
}
\arguments{
\item{x}{numeric vector or matrix}

\item{big.mark, trim, scientific}{options sent to \code{base::format()} but
configured with defaults intended for integer values.}

\item{forceInteger}{logical indicating whether numeric values should
be rounded to the nearest integer value prior to \code{base::format()}.
This option is intended to hide decimal values where they are not
informative.}
}
\value{
character vector if \code{x} is a vector, or if \code{x} is a matrix
a matrix will be returned.
}
\description{
Format an integer as a string
}
\details{
This function is a quick wrapper function around \code{base::format()}
to display integer values as text strings. It will also return a
matrix if the input is a matrix.
}
\examples{
x <- c(1234, 1234.56, 1234567.89);
## By default, commas are used for big.mark, and decimal values are hidden
formatInt(x);

## By default, commas are used for big.mark
formatInt(x, forceInteger=FALSE);

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
\code{\link{gsubOrdered}()},
\code{\link{gsubs}()},
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
\code{\link{sizeAsNum}()},
\code{\link{tcount}()},
\code{\link{ucfirst}()},
\code{\link{uniques}()}
}
\concept{jam string functions}
