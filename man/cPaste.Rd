% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba-string.r
\name{cPaste}
\alias{cPaste}
\title{paste a list into a delimited vector}
\usage{
cPaste(
  x,
  sep = ",",
  doSort = FALSE,
  makeUnique = FALSE,
  na.rm = FALSE,
  keepFactors = FALSE,
  checkClass = TRUE,
  useBioc = TRUE,
  useLegacy = FALSE,
  honorFactor = TRUE,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{x}{input \code{list} of vectors}

\item{sep}{\code{character} delimiter used to paste multiple values together}

\item{doSort}{\code{logical} indicating whether to sort each vector
using \code{\link[=mixedOrder]{mixedOrder()}}.}

\item{makeUnique}{\code{logical} indicating whether to make each vector in
the input list unique before pasting its values together.}

\item{na.rm}{boolean indicating whether to remove NA values from
each vector in the input list. When \code{na.rm} is \code{TRUE} and a
list element contains only \code{NA} values, the resulting string
will be \code{""}.}

\item{keepFactors}{\code{logical} only used when \code{useLegacy=TRUE} and
\code{doSort=TRUE}; indicating whether to preserve factors,
keeping factor level order. When
\code{keepFactors=TRUE}, if any list element is a \code{factor}, all elements
are converted to factors. Note that this step combines overall
factor levels, and non-factors will be ordered using
\code{base::order()} instead of \code{jamba::mixedOrder()} (for now.)}

\item{useBioc}{\code{logical} indicating whether this function should try
to use \code{S4Vectors::unstrsplit()} when the Bioconductor package
\code{S4Vectors} is installed, otherwise it will use a less
efficient \code{mapply()} operation.}

\item{useLegacy}{\code{logical} indicating whether to enable to previous
legacy process used by \code{cPaste()}.}

\item{honorFactor}{\code{logical} passed to \code{mixedSorts()}, whether any
\code{factor} vector should be sorted in factor level order.
When \code{honorFactor=FALSE} then even \code{factor} vectors are sorted
as if they were \code{character} vectors, ignoring the factor levels.}

\item{...}{additional arguments are passed to \code{mixedOrder()} when
\code{doSort=TRUE}.}
}
\value{
character vector with the same names and in the same order
as the input list \code{x}.
}
\description{
Paste a list of vectors into a character vector, with values
delimited by default with a comma.
}
\details{
This function is essentially a wrapper for \code{\link[S4Vectors:character-utils]{S4Vectors::unstrsplit()}}
except that it also optionally applies uniqueness to each vector
in the list, and sorts values in each vector using \code{\link[=mixedOrder]{mixedOrder()}}.

The sorting and uniqueness is applied to the \code{unlist}ed vector of
values, which is substantially faster than any \code{apply} family function
equivalent. The uniqueness is performed by \code{\link[=uniques]{uniques()}}, which itself
will use \code{S4Vectors::unique()} if available.
}
\examples{
L1 <- list(CA=LETTERS[c(1:4,2,7,4,6)], B=letters[c(7:11,9,3)]);

cPaste(L1);
#               CA                 B
# "A,B,C,D,B,G,D,F"   "g,h,i,j,k,i,c"

cPaste(L1, doSort=TRUE);
#               CA                 B
# "A,B,B,C,D,D,F,G"   "c,g,h,i,i,j,k"

## The sort can be done with convenience function cPasteS()
cPasteS(L1);
#               CA                 B
# "A,B,B,C,D,D,F,G"   "c,g,h,i,i,j,k"

## Similarly, makeUnique=TRUE and cPasteU() are the same
cPaste(L1, makeUnique=TRUE);
cPasteU(L1);
#           CA             B
# "A,B,C,D,G,F" "g,h,i,j,k,c"

## Change the delimiter
cPasteSU(L1, sep="; ")
#                CA                  B
# "A; B; C; D; F; G" "c; g; h; i; j; k"

# test mix of factor and non-factor
L2 <- c(
   list(D=factor(letters[1:12],
      levels=letters[12:1])),
   L1);
L2;
cPasteSU(L2, keepFactors=TRUE);

# tricky example with mix of character and factor
# and factor levels are inconsistent
# end result: factor levels are defined in order they appear
L <- list(entryA=c("miR-112", "miR-12", "miR-112"),
   entryB=factor(c("A","B","A","B"),
      levels=c("B","A")),
   entryC=factor(c("C","A","B","B","C"),
      levels=c("A","B","C")),
   entryNULL=NULL)
L;
cPaste(L);
cPasteU(L);

# by default keepFactors=FALSE, which means factors are sorted as characters
cPasteS(L);
cPasteSU(L);
# keepFactors=TRUE will keep unique factor levels in the order they appear
# this is the same behavior as unlist(L[c(2,3)]) on a list of factors
cPasteSU(L, keepFactors=TRUE);
levels(unlist(L[c(2,3)]))

}
\seealso{
Other jam string functions: 
\code{\link{asSize}()},
\code{\link{breaksByVector}()},
\code{\link{cPasteSU}()},
\code{\link{cPasteS}()},
\code{\link{cPasteUnique}()},
\code{\link{cPasteU}()},
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
\code{\link{padInteger}()},
\code{\link{padString}()},
\code{\link{pasteByRowOrdered}()},
\code{\link{pasteByRow}()},
\code{\link{sizeAsNum}()},
\code{\link{tcount}()},
\code{\link{ucfirst}()},
\code{\link{uniques}()}

Other jam list functions: 
\code{\link{cPasteSU}()},
\code{\link{cPasteS}()},
\code{\link{cPasteUnique}()},
\code{\link{cPasteU}()},
\code{\link{heads}()},
\code{\link{jam_rapply}()},
\code{\link{list2df}()},
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
\concept{jam string functions}
