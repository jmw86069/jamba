% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jamba.r
\name{handleArgsText}
\alias{handleArgsText}
\title{Handle function arguments as text}
\usage{
handleArgsText(
  argTextA,
  name = "",
  col1 = "mediumpurple2",
  col2 = "mediumaquamarine",
  colT = "dodgerblue3",
  colF = "red1",
  colNULL = "grey60",
  lightMode = NULL,
  Crange = getOption("jam.Crange"),
  Lrange = getOption("jam.Lrange"),
  adjustRgb = getOption("jam.adjustRgb"),
  indent = "",
  useCollapseList = ",\\n      ",
  useCollapseBase = ", ",
  level = 1,
  debug = 0,
  useColor = TRUE,
  verbose = FALSE,
  ...
)
}
\description{
Handles a list or list of lists, converting to human-readable text format
}
\details{
This function is a rare non-exported function intended to be called by
\code{jargs()}, but separated in order to help isolate the logical
steps required.
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
\code{\link{sdim}()},
\code{\link{setCLranges}()},
\code{\link{setPrompt}()},
\code{\link{ssdima}()},
\code{\link{ssdim}()}
}
\concept{jam practical functions}
