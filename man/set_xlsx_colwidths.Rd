% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba-export.r
\name{set_xlsx_colwidths}
\alias{set_xlsx_colwidths}
\title{Set column widths in Xlsx files}
\usage{
set_xlsx_colwidths(
  xlsxFile,
  sheet = 1,
  cols = seq_along(widths),
  widths = 11,
  ...
)
}
\arguments{
\item{xlsxFile}{\code{character} filename to a file with ".xlsx" extension,
or \code{Workbook} object defined in the \code{openxlsx} package. When
\code{xlsxFile} is a \code{Workbook} the output is not saved to a file.}

\item{sheet}{\code{integer} sheet number or \code{character} sheet name,
passed to \code{openxlsx::setColWidths()} indicating
the worksheet to affect.}

\item{cols}{\verb{integer vector} indicating the column numbers to affect.}

\item{widths}{\verb{numeric vector} indicating the width of each column
defined by \code{cols}.}

\item{...}{additional arguments are passed to \code{openxlsx::setColWidths()}.}
}
\value{
\code{Workbook} object as defined by the \code{openxlsx} package
is returned invisibly with \code{invisible()}. This \code{Workbook}
can be used in argument \code{wb} to provide a speed boost when
saving multiple sheets to the same file.
}
\description{
Set column widths in Xlsx files
}
\details{
This function is a light wrapper to perform these steps
from the very useful \code{openxlsx} R package:
\itemize{
\item \code{openxlsx::loadWorkbook()}
\item \code{openxlsx::setColWidths()}
\item \code{openxlsx::saveWorkbook()}
}
}
\examples{
# write to tempfile for examples
if (check_pkg_installed("openxlsx")) {
   out_xlsx <- tempfile(pattern="writeOpenxlsx_", fileext=".xlsx")
   df <- data.frame(a=LETTERS[1:5], b=1:5);
   writeOpenxlsx(x=df,
      file=out_xlsx,
      sheetName="jamba_test");

   ## By default, cols starts at column 1 and continues to length(widths)
   set_xlsx_colwidths(out_xlsx,
      sheet="jamba_test",
      widths=rep(20, ncol(df))
   )
}

}
\seealso{
Other jam export functions: 
\code{\link{applyXlsxCategoricalFormat}()},
\code{\link{applyXlsxConditionalFormat}()},
\code{\link{readOpenxlsx}()},
\code{\link{set_xlsx_rowheights}()},
\code{\link{writeOpenxlsx}()}
}
\concept{jam export functions}
