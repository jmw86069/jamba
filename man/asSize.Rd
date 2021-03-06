% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba.r
\name{asSize}
\alias{asSize}
\title{convert numeric value to size}
\usage{
asSize(
  x,
  humanFriendly = TRUE,
  digits = 3,
  abbreviateUnits = TRUE,
  unitType = "bytes",
  unitAbbrev = gsub("^(.).*$", "\\\\1", unitType),
  kiloSize = 1024,
  sep = " ",
  ...
)
}
\arguments{
\item{x}{numeric vector}

\item{humanFriendly}{logical, currently only TRUE is accepted, whether to
include human-friendly units to the output.}

\item{digits}{integer number of digits used by \code{\link[base]{format}} when
formatting the number to create a character string}

\item{abbreviateUnits}{logical whether to print abbreviated units, for
example using k, M, G, T, P instead of kilo, mega, Giga, Tera, Peta,
respectively.}

\item{unitType}{character string indicating the base unit of measure,
by default "bytes". Note that trailing "s" is removed when the number
is singular.}

\item{unitAbbrev}{character string indicating an abbreviated base unit,
by default it uses the first character from \code{unitType}.}

\item{kiloSize}{numeric number of base units when converting from one
base unit, to one "kilo" base unit. For file sizes, this value is 1024,
but for other purposes this value may be 1000.}

\item{sep}{delimiter used between the numeric value and the unit.}

\item{...}{other parameters passed to \code{\link[base]{format}}.}
}
\value{
character vector representing human-friendly sizes.
}
\description{
convert numeric value to size
}
\details{
This function converts numeric input to computer size in bytes, using
the most appropriate human-friendly units as possible. For example, it
will use kilobytes (kb), megabytes (Mb), gigabytes (Gb), etc. as
necessary.

This function by default assumes 1,024 bytes per kilobyte, but can be
made to use 1000, for example in order to represent genome base size,
which is represented in kilobases (kb), megabases (Mb), gigabases (Gb),
etc. Simply define unitType="bases" in this scenario.

TODO: Allow units to be specified as a function argument, for
example "million" and "billion" instead of "Giga" and "Tera".
}
\examples{
asSize(c(1, 10,2010,22000,52200))
#> "1 byte"   "10 bytes" "2 kb"     "21 kb"    "51 kb"

# demonstration of straight numeric units
asSize(c(1, 100, 1000, 10000), unitType="", kiloSize=100)

}
\seealso{
Other jam string functions: 
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
\code{\link{tcount2}()},
\code{\link{tcount}()},
\code{\link{ucfirst}()},
\code{\link{uniques}()}
}
\concept{jam string functions}
