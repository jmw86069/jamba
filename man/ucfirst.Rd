% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba-string.r
\name{ucfirst}
\alias{ucfirst}
\title{Uppercase the first letter in each word}
\usage{
ucfirst(x, lowercaseAll = FALSE, firstWordOnly = FALSE, ...)
}
\arguments{
\item{x}{character vector.}

\item{lowercaseAll}{logical indicating whether to force all letters
to lowercase before applying uppercase to the first letter.}

\item{firstWordOnly}{logical indicating whether to apply the
uppercase only to the first word in each string. Note that it
still applies the logic to every entry in the input vector \code{x}.}

\item{...}{additional arguments are ignored.}
}
\value{
\code{character} vector where letters are converted to uppercase.
}
\description{
Uppercase the first letter in each word
}
\details{
This function is a simple mimic of the Perl function \code{ucfirst} which
converts the first letter in each word to uppercase. When
\code{lowercaseAll=TRUE} it also forces all other letters to lowercase,
otherwise mixedCase words will retain capital letters in the middle
of words.
}
\examples{
ucfirst("TESTING_ALL_UPPERCASE_INPUT")
ucfirst("TESTING_ALL_UPPERCASE_INPUT", TRUE)
ucfirst("TESTING_ALL_UPPERCASE_INPUT", TRUE, TRUE)

ucfirst("testing mixedCase upperAndLower case input")
ucfirst("testing mixedCase upperAndLower case input", TRUE)
ucfirst("testing mixedCase upperAndLower case input", TRUE, TRUE)

}
\seealso{
Other jam string functions: 
\code{\link{asSize}()},
\code{\link{breaksByVector}()},
\code{\link{fillBlanks}()},
\code{\link{formatInt}()},
\code{\link{gsubOrdered}()},
\code{\link{gsubs}()},
\code{\link{makeNames}()},
\code{\link{nameVector}()},
\code{\link{nameVectorN}()},
\code{\link{padInteger}()},
\code{\link{padString}()},
\code{\link{pasteByRow}()},
\code{\link{pasteByRowOrdered}()},
\code{\link{sizeAsNum}()},
\code{\link{tcount}()}
}
\concept{jam string functions}
