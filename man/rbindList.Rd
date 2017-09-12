% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba-string.r
\name{rbindList}
\alias{rbindList}
\title{rbind a list of vectors into matrix or data.frame}
\usage{
rbindList(x, emptyValue = "", keepListNames = TRUE, newColnames = NULL,
  newRownames = NULL, fixBlanks = TRUE, returnDF = FALSE,
  verbose = FALSE, ...)
}
\arguments{
\item{x}{input list of vectors.}

\item{emptyValue}{character value to use to represent missing values,
whenever a blank cell is introduced into the resulting matrix}

\item{keepListNames}{logical whether to use list names as rownames
in the resulting matrix or data.frame.}

\item{newColnames}{NULL or character vector of colnames to use for the
resulting matrix or data.frame.}

\item{newRownames}{NULL or character vector of rownames to use for the
resulting matrix or data.frame. If supplied, this value overrides the
keepListNames=TRUE use of list names as rownames.}

\item{fixBlanks}{logical whether to use blank values instead of repeating
each vector to the length of the maximum vector length when filling
each row of the matrix or data.frame.}

\item{returnDF}{logical whether to return a data.frame, by default FALSE,
a matrix is returned.}

\item{verbose}{logical whether to print verbose output during processing.}
}
\value{
By default a matrix, or if returnDF=TRUE the object is coerced to a
data.frame. The rownames by default are derived from the list names,
but the colnames are not derived from the vector names.
}
\description{
rbind a list of vectors into matrix or data.frame
}
\details{
The purpose of this function is to emulate do.call(rbind, x) on a list
of vectors, while specifically handling when there are different
numbers of entries per vector. Instead of repeating the values to fill the
number of resulting columns, this function fills cells with blank fields.
In extensive timings tests at the time this function was created, this
method was notably faster than alternatives. That is, it runs
\code{do.call(rbind, x)} then subsequent steps to replace values with
blank entries was notably faster than other alternatives.
}
\examples{
L <- list(a=LETTERS[1:4], b=letters[1:3]);
do.call(rbind, L);
rbindList(L);
rbindList(L, returnDF=TRUE);

}