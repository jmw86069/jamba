% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba.r
\name{sdim}
\alias{sdim}
\title{print dimensions of list object elements}
\usage{
sdim(
  x,
  includeClass = TRUE,
  doFormat = FALSE,
  big.mark = ",",
  verbose = FALSE,
  ...
)
}
\arguments{
\item{x}{an S3 object inheriting from class "list", or an S4 object.}

\item{includeClass}{boolean indicating whether to print the class of
each element in the input \code{x} object. Note that for S4 objects,
each element will be the object returned for each of \code{slotNames(x)}.}

\item{doFormat}{boolean indicating whether to format the dimensions using
\code{format(...,big.mark=",")}, which is mainly useful for extremely
large dimensions. This parameter should probably become more broadly
useful and respectful for different locales.}

\item{big.mark}{character value used when \code{doFormat=TRUE}, used in the
call to \code{format(...,big.mark)}.}

\item{verbose}{logical whether to print verbose output}

\item{...}{additional parameters are ignored.}
}
\value{
\code{data.frame} where each row indicates the dimensions of
each element in the input list. When \code{includeClass} is \code{TRUE} it
will include a column \code{class} which indicates the class of each
list element. When the input list contains arrays with more than
two dimensions, the first two dimensions are named \code{"rows"} and
\code{"columns"} with additional dimensions named \code{"dim3"} and so on.
Any list element with fewer than that many dimensions will only have
values populated to the relevant dimensions, for example a character
vector will only populate the length.
}
\description{
print dimensions of list object elements, such as a list of data.frames
}
\details{
This function prints the dimensions of a list of objects, usually a list
of data.frames, but extended to handle more complicated lists, including
even S4 object slotNames().

Over time, more object types will be made compatible with this function.
Currently, igraph objects will print the number of nodes and edges, but
requires the igraph package to be installed.
}
\examples{
L <- list(LETTERS=LETTERS,
   letters=letters,
   lettersDF=data.frame(LETTERS, letters));
sdim(L);

}
\seealso{
Other jam practical functions: 
\code{\link{applyCLrange}()},
\code{\link{breakDensity}()},
\code{\link{checkLightMode}()},
\code{\link{check_pkg_installed}()},
\code{\link{colNum2excelName}()},
\code{\link{exp2signed}()},
\code{\link{fileInfo}()},
\code{\link{fixYellowHue}()},
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
\code{\link{printDebugI}()},
\code{\link{printDebug}()},
\code{\link{renameColumn}()},
\code{\link{rmInfinite}()},
\code{\link{rmNA}()},
\code{\link{rmNULL}()},
\code{\link{sclass}()},
\code{\link{sdima}()},
\code{\link{setCLranges}()},
\code{\link{setPrompt}()},
\code{\link{ssdima}()},
\code{\link{ssdim}()}

Other jam list functions: 
\code{\link{cPasteSU}()},
\code{\link{cPasteS}()},
\code{\link{cPasteUnique}()},
\code{\link{cPasteU}()},
\code{\link{cPaste}()},
\code{\link{heads}()},
\code{\link{jam_rapply}()},
\code{\link{list2df}()},
\code{\link{mergeAllXY}()},
\code{\link{mixedSorts}()},
\code{\link{rbindList}()},
\code{\link{relist_named}()},
\code{\link{rlengths}()},
\code{\link{sclass}()},
\code{\link{sdima}()},
\code{\link{ssdima}()},
\code{\link{ssdim}()},
\code{\link{uniques}()},
\code{\link{unnestList}()}
}
\concept{jam list functions}
\concept{jam practical functions}
