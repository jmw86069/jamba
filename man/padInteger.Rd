% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba.r
\name{padInteger}
\alias{padInteger}
\title{prefix integers with leading zeros}
\usage{
padInteger(x, padCharacter = "0", useNchar = NULL, ...)
}
\arguments{
\item{x}{\code{integer}, \code{numeric}, or \code{character} vector. In reality, only
\code{nchar(x)} is required to determine padding.}

\item{padCharacter}{\code{character} with nchar(padCharacter)==1, used to pad
each digit as a prefix.}

\item{useNchar}{\code{NULL} or \code{integer} number of digits used, or if the maximum
\code{nchar(x)} is higher, that number of digits is used. Note \code{useNchar} is
mostly useful when all numbers are less than 10, but the desired output
is to have a fixed number of digits 2 or higher.}

\item{...}{additional parameters are ignored.}
}
\value{
\code{character} vector of length(x).
}
\description{
prefix integers with leading zeros
}
\details{
The purpose of this function is to pad integer numbers so they contain
a consistent number of digits, which is helpful when sorting values
as character strings.
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
\code{\link{gsubs}()},
\code{\link{makeNames}()},
\code{\link{mixedOrder}()},
\code{\link{mixedSortDF}()},
\code{\link{mixedSorts}()},
\code{\link{mixedSort}()},
\code{\link{mmixedOrder}()},
\code{\link{nameVectorN}()},
\code{\link{nameVector}()},
\code{\link{padString}()},
\code{\link{pasteByRowOrdered}()},
\code{\link{pasteByRow}()},
\code{\link{sizeAsNum}()},
\code{\link{tcount}()},
\code{\link{ucfirst}()},
\code{\link{uniques}()}
}
\concept{jam string functions}
