% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba-heatmap-order.R
\name{heatmap_column_order}
\alias{heatmap_column_order}
\title{Return Heatmap column order from ComplexHeatmap heatmap object}
\usage{
heatmap_column_order(hm)
}
\arguments{
\item{hm}{\code{Heatmap} or \code{HeatmapList} object as defined by the
Bioconductor package via \code{ComplexHeatmap::Heatmap()}.}
}
\value{
output depends upon the heatmap:
\itemize{
\item When heatmap columns are grouped using \code{column_split}, and
when the data matrix contains colnames,
returns a \code{character} vector of colnames in the order they
appear in the heatmap. When there are no colnames, \code{integer}
column index values are returned. If the heatmap has column labels,
they are returned as vector names.
\item When columns are grouped using \code{column_split}, it returns a
\code{list} of vectors as described above. The \code{list}
is named using the \code{column_title} labels only when there is
an equal number of column labels.
}
}
\description{
Return Heatmap column order from ComplexHeatmap heatmap object
}
\details{
This function is a helpful utility to return the fully
qualified list of colnames in a \code{ComplexHeatmap::Heatmap}
object.

The core intention is for the output to be usable with the
original data matrix used in the heatmap. Therefore, the
vector values are \code{colnames()} when present, or \code{integer}
column index values when there are no \code{colnames()}. If heatmap
\code{column_labels} are defined, they are returned as \code{names()}.

Note that \code{names()} are assigned inside \code{try()} to allow the
case where \code{column_labels}, or \code{column_title} labels cannot be
coerced to \code{character} values, for example using \code{gridtext}
for markdown formatting.
}
\seealso{
Other jam utility functions: 
\code{\link{heatmap_row_order}()}
}
\concept{jam utility functions}