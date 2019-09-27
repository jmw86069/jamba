% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba.r
\name{mergeAllXY}
\alias{mergeAllXY}
\title{Merge list of data.frames retaining all rows}
\usage{
mergeAllXY(...)
}
\description{
Merge list of data.frames retaining all rows
}
\details{
This function is a wrapper around \code{base::merge.data.frame()}
except that it allows more than two data.frame objects,
and applies default arguments \code{all.x=TRUE} and \code{all.y=TRUE}
for each merge operation to ensure that all rows are kept.
}
\seealso{
Other jam practical functions: \code{\link{applyCLrange}},
  \code{\link{breakDensity}}, \code{\link{checkLightMode}},
  \code{\link{colNum2excelName}}, \code{\link{exp2signed}},
  \code{\link{fileInfo}}, \code{\link{fixYellowHue}},
  \code{\link{fixYellow}}, \code{\link{getAxisLabel}},
  \code{\link{handleArgsText}}, \code{\link{isFALSEV}},
  \code{\link{isTRUEV}}, \code{\link{jamba}},
  \code{\link{jargs}}, \code{\link{kable_coloring}},
  \code{\link{log2signed}}, \code{\link{make_styles}},
  \code{\link{minorLogTicks}}, \code{\link{newestFile}},
  \code{\link{printDebug}}, \code{\link{renameColumn}},
  \code{\link{rmInfinite}}, \code{\link{rmNA}},
  \code{\link{rmNULL}}, \code{\link{sclass}},
  \code{\link{sdim}}, \code{\link{setCLranges}},
  \code{\link{setPrompt}}, \code{\link{ssdim}}

Other jam list functions: \code{\link{cPasteSU}},
  \code{\link{cPasteS}}, \code{\link{cPasteUnique}},
  \code{\link{cPasteU}}, \code{\link{cPaste}},
  \code{\link{list2df}}, \code{\link{mixedSorts}},
  \code{\link{rbindList}}, \code{\link{relist_named}},
  \code{\link{rlengths}}, \code{\link{sclass}},
  \code{\link{sdim}}, \code{\link{ssdim}},
  \code{\link{uniques}}, \code{\link{unnestList}}
}
\concept{jam list functions}
\concept{jam practical functions}